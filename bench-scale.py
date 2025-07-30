#!/usr/bin/env python3

import subprocess
import sys
import os
import argparse
import tempfile
from pathlib import Path

def generate_types(m):
    """Generate M different types for overloading."""
    assert m >= 2, "M must be at least 2"
    
    base_types = ["Int", "String", "Double", "Bool", "Char"]
    
    if m <= len(base_types):
        return base_types[:m]
    
    # Additional types to fill the ranks
    types = base_types.copy()
    for i in range(len(base_types), m):
        types.append(f"Type{i}")
    
    return types

def generate_overloading_program(n, m, solution_type):
    """Generate a program with N add calls, M overloads, and specified solution count."""
    assert n > 0, "N must be positive"
    assert m >= 2, "M must be at least 2"
    assert solution_type in ["zero", "one", "many"], f"Invalid solution type: {solution_type}"
    
    types = generate_types(m)
    
    # Generate assume statements for each overload
    assumes = []
    for type_name in types:
        func_name = f"add{type_name}"
        assumes.append(f"assume {func_name}: ({type_name}, {type_name}) => {type_name};")
    
    # Generate the overloads
    overload_funcs = [f"add{t}" for t in types]
    overload_set = f"assume add = <{', '.join(overload_funcs)}>;"
    
    # Generate lambda expression: λx => x + x + x + ... (N times)
    if solution_type == "many":
        expr = "x"
        for _ in range(n - 1):
            expr = f"add(x, {expr})"
        lambda_expr = f"λ(x) => add(x, {expr})"
    
    elif solution_type == "one":
        # Force exactly one solution by adding 0 at the end
        if n == 1:
            lambda_expr = f"λ(x) => add(x, 0)"
        else:
            expr = "0" # hardcoded '... + 0'
            for _ in range(n - 1):
                expr = f"add(x, {expr})"
            lambda_expr = f"λ(x) => add(x, {expr})"
    
    else:  # solution_type == "zero"
        # Create type conflicts: λx => x + ... + 0 + "hello"
        if n == 1:
            lambda_expr = f'λ(x) => add(add(x, 0), "hello")'
        else:
            expr = 'add(0, "hello")' # hardcoded '... + 0 + hello'
            for _ in range(n - 2):
                expr = f"add(x, {expr})"
            lambda_expr = f'λ(x) => add(x, {expr})'
    
    # Combine all parts
    program_parts = assumes + [overload_set, "", lambda_expr]
    return "\n".join(program_parts)

def create_temp_file(content, suffix=".over"):
    """Create a temporary file with the given content."""
    with tempfile.NamedTemporaryFile(mode='w', suffix=suffix, delete=False) as f:
        f.write(content)
        return f.name

def create_benchmark(n_values, m_values, solution_types, interpreter_cmd, 
                              warmup=2, runs=10):
    """Create a single hyperfine command that compares all parameter combinations."""
    commands = []
    command_names = []
    temp_files = []

    try:
        total_combinations = len(n_values) * len(m_values) * len(solution_types)
        print(f"Generating {total_combinations} test combinations...")
        
        for n in n_values:
            for m in m_values:
                for solution_type in solution_types:
                    # Generate program
                    code = generate_overloading_program(n, m, solution_type)
                    
                    # Create temporary file
                    temp_file = create_temp_file(code)
                    temp_files.append(temp_file)
                    
                    # Build command and name
                    cmd = interpreter_cmd.replace('{}', temp_file)
                    name = f"N={n}_M={m}_{solution_type}"
                    
                    commands.append(cmd)
                    command_names.append(name)

        if not commands:
            print("No commands to benchmark!")
            return False

        # Build hyperfine command
        cmd = [
            'hyperfine',
            '--shell=none',
            '--warmup', str(warmup),
            '--runs', str(runs),
            '--export-csv', 'results_scale.csv',
            '--export-markdown', 'results_scale.md'
        ]

        # Add command names and commands
        for name, command in zip(command_names, commands):
            cmd.extend(['--command-name', name, command])

        print(f"Running comparison benchmark with {len(commands)} combinations...")

        try:
            subprocess.run(cmd, check=True)
            print(f"✓ Benchmark completed successfully")
            print(f"Results saved to results_scale.{{csv,md}}")
            return True
        except subprocess.CalledProcessError as e:
            print(f"✗ Benchmark failed: {e}")
            return False

    finally:
        # Clean up temporary files
        for temp_file in temp_files:
            try:
                os.unlink(temp_file)
            except OSError:
                pass

def main():
    parser = argparse.ArgumentParser(description='Benchmark overloading resolution performance')
    parser.add_argument('--n-values', nargs='+', type=int, default=[10, 50, 100, 150, 200],
                        help='List of N values (number of add calls, default: 10 50 100 150 200)')
    parser.add_argument('--m-values', nargs='+', type=int, default=[10, 25, 50, 75, 100], 
                        help='List of M values (number of overloads, default: 10 25 50 75 100)')
    parser.add_argument('--solutions', nargs='+', choices=['zero', 'one', 'many'],
                        default=['zero', 'one', 'many'],
                        help='Solution types to test (default: zero one many)')
    parser.add_argument('--interpreter-cmd', type=str, required=True,
                        help='Command to run interpreter (use {} as placeholder for input file)')
    parser.add_argument('--warmup', type=int, default=2,
                        help='Number of warmup runs (default: 2)')
    parser.add_argument('--runs', type=int, default=10,
                        help='Number of benchmark runs (default: 10)')

    args = parser.parse_args()

    print(f"Benchmark configuration:")
    print(f"  N values: {args.n_values}")
    print(f"  M values: {args.m_values}")
    print(f"  Solution types: {args.solutions}")
    print(f"  Warmup runs: {args.warmup}")
    print(f"  Benchmark runs: {args.runs}")
    print(f"  Interpreter: {args.interpreter_cmd}")
    print()

    success = create_benchmark(
        args.n_values, args.m_values, args.solutions, args.interpreter_cmd,
        args.warmup, args.runs
    )
    
    if success:
        print(f"\n✓ All benchmarks completed! Check results_scale.{{csv,md}} for results.")
    else:
        print(f"\n✗ Benchmarks failed!")
        sys.exit(1)

if __name__ == "__main__":
    main()
