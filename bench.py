#!/usr/bin/env python3

import subprocess
import sys
import os
import argparse
import tempfile
import string

def generate_variable_name(index):
    """Generate variable names: a, b, c, ..., z, aa, bb, cc, ..."""
    if index < 26:
        return string.ascii_lowercase[index]
    else:
        letter = string.ascii_lowercase[index % 26]
        return letter * ((index // 26) + 1)

def generate_over_language(n):
    """Generate the 'over' language benchmark with n compose calls."""
    if n < 2:
        raise ValueError("n must be at least 2")

    variables = []
    for i in range(n):
        var_name = generate_variable_name(i)
        variables.append(f"{var_name}<addInt, addString>")

    if n == 2:
        result = f"compose({variables[0]}, {variables[1]})"
    else:
        result = f"compose({variables[0]}, {variables[1]})"
        for i in range(2, n):
            result = f"compose({result}, {variables[i]})"

    return f"""
assume addString: (String) => Int;
assume addInt: (Int) => Int;
assume callString: ∀(R). ((String) => R) => R;
assume callInt: ∀(R). ((Int) => R) => R;
assume compose: ∀(R). ((R) => R, (R) => R) => ((R) => R);

call<callInt,callString>({result})"""

def generate_swift(n):
    """Generate the Swift benchmark with n compose calls."""
    if n < 2:
        raise ValueError("n must be at least 2")

    if n == 2:
        result = "compose(add, add)"
    else:
        result = "compose(add, add)"
        for i in range(2, n):
            result = f"compose({result}, add)"

    res = f"""
struct MyInt {{ let value: Int }}

struct MyString {{ let value: String }}

func add(_ n: MyInt) -> MyInt {{ MyInt(value: n.value + 1) }}
func add(_ n: MyString) -> MyInt {{ MyInt(value: 1) }}

func call<R>(_ f: (MyInt) -> R) -> R {{ f(MyInt(value: 1)) }}
func call<R>(_ f: (MyString) -> R) -> R {{ f(MyString(value: "hello")) }}

func compose<R>(_ f: @escaping (R) -> R, _ g: @escaping (R) -> R) -> (R) -> R {{
    return f
}}
print(call({result}))"""
    return res

def create_temp_file(content, suffix=".txt"):
    """Create a temporary file with the given content."""
    with tempfile.NamedTemporaryFile(mode='w', suffix=suffix, delete=False) as f:
        f.write(content)
        return f.name

def run_benchmark(command, benchmark_name, warmup=3, runs=10):
    """Run a single benchmark using hyperfine."""
    cmd = [
        'hyperfine',
        '--ignore-failure',
        # '--show-output',
        '--warmup', str(warmup),
        '--runs', str(runs),
        '--export-json', f'{benchmark_name}_results.json',
        '--export-markdown', f'{benchmark_name}_results.md',
        command
    ]

    print(f"Running benchmark: {benchmark_name}")
    print(f"Command: {' '.join(cmd)}")

    try:
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        print(f"Benchmark {benchmark_name} completed successfully")
        return True
    except subprocess.CalledProcessError as e:
        print(f"Benchmark {benchmark_name} failed:")
        print(f"  stdout: {e.stdout}")
        print(f"  stderr: {e.stderr}")
        return False
    except FileNotFoundError:
        print("Hyperfine not found!")
        return False

def run_scaling_benchmarks(language, sizes, compiler_cmd, warmup=3, runs=10):
    """Run benchmarks for different input sizes."""
    results = []
    temp_files = []

    try:
        for size in sizes:
            print(f"\n--- Benchmarking {language} with size {size} ---")

            # Generate the benchmark code
            if language == 'over':
                code = generate_over_language(size)
                suffix = '.over'
            elif language == 'swift':
                code = generate_swift(size)
                suffix = '.swift'
            else:
                raise ValueError(f"Unknown language: {language}")

            # Create temporary file
            temp_file = create_temp_file(code, suffix)
            temp_files.append(temp_file)

            # Build the command
            cmd = compiler_cmd.replace('{}', temp_file)
            benchmark_name = f"{language}_size_{size}"

            # Run the benchmark
            success = run_benchmark(cmd, benchmark_name, warmup, runs)
            results.append({
                'language': language,
                'size': size,
                'success': success,
                'benchmark_name': benchmark_name
            })

            if not success:
                print(f"Stopping benchmarks due to failure at size {size}")
                break

    finally:
        # Clean up temporary files
        for temp_file in temp_files:
            try:
                os.unlink(temp_file)
            except OSError:
                pass

    return results

def compare_languages(sizes, over_cmd, swift_cmd, warmup=3, runs=10):
    """Compare different languages for the same input sizes."""
    print("=== Running Language Comparison Benchmarks ===\n")

    all_results = []

    # Run over language benchmarks
    if over_cmd:
        print("Running 'over' language benchmarks...")
        over_results = run_scaling_benchmarks('over', sizes, over_cmd, warmup, runs)
        all_results.extend(over_results)

    # Run Swift benchmarks
    if swift_cmd:
        print("\nRunning Swift benchmarks...")
        swift_results = run_scaling_benchmarks('swift', sizes, swift_cmd, warmup, runs)
        all_results.extend(swift_results)

    return all_results

def create_comparison_benchmark(sizes, over_cmd, swift_cmd, output_name="comparison", warmup=3, runs=10):
    """Create a single hyperfine command that compares all benchmarks."""
    commands = []
    command_names = []
    temp_files = []

    try:
        for size in sizes:
            if over_cmd:
                over_code = generate_over_language(size)
                over_file = create_temp_file(over_code, '.over')
                temp_files.append(over_file)
                commands.append(over_cmd.replace('{}', over_file))
                command_names.append(f"over_size_{size}")

            if swift_cmd:
                swift_code = generate_swift(size)
                swift_file = create_temp_file(swift_code, '.swift')
                temp_files.append(swift_file)
                commands.append(swift_cmd.replace('{}', swift_file))
                command_names.append(f"swift_size_{size}")

        if not commands:
            print("No commands to benchmark!")
            return False

        # Build hyperfine command
        cmd = [
            'hyperfine',
            '--ignore-failure',
            # '--show-output',
            '--warmup', str(warmup),
            '--runs', str(runs),
            '--export-json', f'{output_name}_comparison.json',
            '--export-markdown', f'{output_name}_comparison.md'
        ]

        # Add command names
        for name in command_names:
            cmd.extend(['--command-name', name])

        # Add commands
        cmd.extend(commands)

        print(f"Running comparison benchmark with {len(commands)} commands...")
        print(f"Command: {' '.join(cmd[:10])}..." if len(cmd) > 10 else f"Command: {' '.join(cmd)}")

        try:
            result = subprocess.run(cmd, check=True)
            print(f"✓ Comparison benchmark completed successfully")
            print(f"Results saved to {output_name}_comparison.json and {output_name}_comparison.md")
            return True
        except subprocess.CalledProcessError as e:
            print(f"✗ Comparison benchmark failed: {e}")
            return False

    finally:
        # Clean up temporary files
        for temp_file in temp_files:
            try:
                os.unlink(temp_file)
            except OSError:
                pass

def main():
    parser = argparse.ArgumentParser(description='Benchmark runner for type inference systems')
    parser.add_argument('--sizes', nargs='+', type=int, default=[2, 4, 6, 8, 10, 12, 14, 16],
                        help='List of compose sizes to benchmark (default: 2 4 6 8 10 12 14 16)')
    parser.add_argument('--over-cmd', type=str,
                        help='Command to run over compiler (use {} as placeholder for input file)')
    parser.add_argument('--swift-cmd', type=str,
                        help='Command to run Swift compiler (use {} as placeholder for input file)',
                        default='timeout 600s swiftc {} -o /tmp/swift_bench')
    parser.add_argument('--warmup', type=int, default=2,
                        help='Number of warmup runs (default: 2)')
    parser.add_argument('--runs', type=int, default=5,
                        help='Number of benchmark runs (default: 5)')
    parser.add_argument('--output', type=str, default='benchmark',
                        help='Output prefix for result files (default: benchmark)')
    parser.add_argument('--mode', choices=['individual', 'comparison'], default='comparison',
                        help='Run mode: individual (separate benchmarks) or comparison (single hyperfine run)')
    parser.add_argument('--generate-only', action='store_true',
                        help='Only generate code samples, don\'t run benchmarks')

    args = parser.parse_args()

    if not args.over_cmd and not args.swift_cmd:
        print("Error: At least one of --over-cmd or --swift-cmd must be specified")
        sys.exit(1)

    print(f"Benchmark configuration:")
    print(f"  Sizes: {args.sizes}")
    print(f"  Warmup runs: {args.warmup}")
    print(f"  Benchmark runs: {args.runs}")
    print(f"  Mode: {args.mode}")
    if args.over_cmd:
        print(f"  Over command: {args.over_cmd}")
    if args.swift_cmd:
        print(f"  Swift command: {args.swift_cmd}")
    print()

    if args.generate_only:
        print("=== Generated Code Samples ===")
        for size in args.sizes:
            print(f"\n--- Size {size} ---")
            if args.over_cmd:
                print(f"Over: {generate_over_language(size)}")
            if args.swift_cmd:
                print(f"Swift: {generate_swift(size)}")
        return

    if args.mode == 'comparison':
        success = create_comparison_benchmark(
            args.sizes, args.over_cmd, args.swift_cmd,
            args.output, args.warmup, args.runs
        )
        if success:
            print(f"\n✓ All benchmarks completed! Check {args.output}_comparison.{{md, json}} for results.")
        else:
            print(f"\n✗ Benchmarks failed!")
            sys.exit(1)
    else:
        results = compare_languages(
            args.sizes, args.over_cmd, args.swift_cmd,
            args.warmup, args.runs
        )

        # Print summary
        print(f"\n=== Benchmark Summary ===")
        for result in results:
            status = "✓" if result['success'] else "✗"
            print(f"{status} {result['language']} size {result['size']}: {result['benchmark_name']}")

if __name__ == "__main__":
    main()
