# Artifact of the paper 'The Simple Essence of Overloading'

> **Availability:** We will make our artifact available via Zenodo.

This GitHub repository contains the artifact of the paper:

> The Simple Essence of Overloading:
> Making ad-hoc polymorphism more algebraic with flow-based variational type-checking

## Overview

This artifact consists of:
- a prototype implementation of the calculus and the type inference pipeline (`.#native`),
- a web UI where a reader can input a term in Variational Core and get the rendered resulting type, constraints, and bounds of the calculus,
in addition to the result of overload resolution (`.#website`),
- a benchmark suite comparing performance against Swift (`#perf-benchmark`) plus a scalability analysis (`#scale-benchmark`),
- and of a development environment for developing and testing the above.

All of the above is packaged using _Nix_, the packaging is declared in [flake.nix](./flake.nix).

### Paper Claims

The paper makes the following claims:

1. We present a flow-based variational framework that provides a semantic model of overload resolution as a search for a single possible world without type errors (Section 2), along with a formal variational calculus with type system and semantics (Section 3).
    - **Supported:** The artifact provides a complete prototype implementation of the variational calculus and type system in Rust, demonstrating all examples from the paper through both the native CLI and interactive web interface.
2. We formalize modular overload resolution with clear separation into distinct phases: constraint collection, constraint solving, overload resolution, and program specialization (Sections 4.1, 4.2, 4.4).
    - **Supported:** The prototype implementation in the artifact demonstrates constraint collection, solving, and overload resolution with clear phase separation as formalized in the paper. However, operational semantics and program specialization are **not** implemented, as we consider these to be routine transformations that do not demonstrate the novel contributions of the paper. Note that we also do not provide machine-checked proofs of these claims.
3. Our approach avoids typical problems of backtracking implementations through type-directed overload resolution with clear phase separation (Section 4).
    - **Supported:** The artifact demonstrates this through the prototype implementation, which uses the variational framework to avoid backtracking during overload resolution in a modular fashion, cleanly separating out overload resolution from constraint gathering and solving.
4. The performance and usability of our approach is viable in practice. (Section 5)
    - **Supported:** This is currently demonstrated through the performance benchmarks against Swift and through the scaling benchmarks, and through the interactive web interface demonstrating usability.

## Hardware Dependencies

- Tested on ARM64 macOS and x64 Linux
- Requires ≤8 GiB of disk space for Nix & the developer environment

## Getting Started Guide

This guide describes how to set up the dependencies for the artifact and
how to get the core pieces of the artifact up and running quickly.

### Setup

This artifact requires Nix with flakes support.
The Nix flake packages the project and ought to provide everything else needed to fully use, benchmark, and even develop the project.

The project was tested with upstream Nix versions 2.28.1 and 2.29.1 on ARM64 macOS and x64 Linux.

#### Installing Nix with flakes support

To install Nix, we recommend using the Determinate Nix installer available at <https://github.com/DeterminateSystems/nix-installer>
because of its good uninstall and macOS support. As of the time of writing, we recommend the reader to use upstream Nix,
not the Determinate fork of Nix (choosing "No" if the installer asks you if you want specifically "Determinate Nix").

To test your Nix installation and Nix flakes support, try running the following command
```bash
$ nix run nixpkgs#cowsay -- 'LGTM'
```

If this command shows an ASCII rendering of a cow saying `LGTM` in your terminal, then your Nix installation is likely correctly set up.

If you see an error like: `error: experimental Nix feature 'flakes' is disabled`,
you need to enable flakes by adding the following line to `~/.config/nix/nix.conf`:
```conf
experimental-features = nix-command flakes
```

In order to later uninstall Nix, please follow the instructions of `nix-installer` here: https://github.com/DeterminateSystems/nix-installer?tab=readme-ov-file#uninstalling.

### Quick Start

If you have Nix and tested that you have full Nix flakes support, here's how to quickly test out the artifact:

#### 1. Clone the repository

Run the following command to clone the repository and enter the resulting folder:
```bash
$ git clone git@github.com:se-tuebingen/oopsla-2025-artifact-overloading.git
$ cd oopsla-2025-artifact-overloading
```

#### 2. Try the scaling benchmarking suite on a tiny input

Next, run the following Nix command to fetch all necessary dependencies (around 4 GiB of disk space), build the "native" program (around 2 GiB of disk space), run a tiny benchmark, and see its results:
```
$ nix run .#scale-benchmark -- --n-values 5 10 --m-values 10
$ cat results_scale.md
```

This variant of the benchmark runs for about a second on our computer.
You should see hyperfine output, the times should be roughly the same for all `N=5_M=10_$` and the same for all `N=10_M=10_$` (in units of milliseconds).

Please ignore the warning 'Command took less than 5 ms to complete.' if there is one, but please, **do not ignore** if at this point some command failed,
for example if the benchmarking tool returned a warning 'Command terminated with non-zero exit code'.

Finally, the `cat` command should show a Markdown-formatted table for easy overview. Here's an example:

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `N=5_M=10_zero` | 4.0 ± 0.4 | 3.5 | 4.6 | 1.13 ± 0.11 |
| `N=5_M=10_one` | 4.4 ± 2.6 | 3.4 | 11.9 | 1.25 ± 0.75 |
| `N=5_M=10_many` | 3.5 ± 0.1 | 3.3 | 3.7 | 1.00 |
| `N=10_M=10_zero` | 4.9 ± 0.1 | 4.6 | 5.1 | 1.38 ± 0.06 |
| `N=10_M=10_one` | 4.9 ± 0.2 | 4.8 | 5.4 | 1.40 ± 0.07 |
| `N=10_M=10_many` | 4.7 ± 0.2 | 4.6 | 5.1 | 1.34 ± 0.07 |


The benchmarks themselves are described later in section [Benchmarking](#benchmarking).

#### 3. Try out the interactive website

The artifact has its own website which you can bundle up and run locally using the following command.
This will build the WASM target, bundle it with `index.html` and the examples, and start a local web server on port 3000:
```
$ nix run .#website
```

The expected result in the command line is:
```
$ nix run .#website
Starting web server for overloading website...
WASM package directory: /nix/store/a1b2c3d4e5f6g7h8i9jk-overloading-wasm-0.1.0


   ┌──────────────────────────────────────────┐
   │                                          │
   │   Serving!                               │
   │                                          │
   │   - Local:    http://localhost:3000      │
   │   - Network:  http://127.0.0.1:3000      │
   │                                          │
   └──────────────────────────────────────────┘
```

Afterwards, please open your browser on the shown page, here `localhost:3000` and verify that you see the same page as on the image:

![Screenshot of the website as built, showing the input editor and the output.](./img/website-expected.png)

Finally, you can try editing the given example or selecting a different one from the dropdown menu (there should be plenty of examples available).
The output changes on every change in the editor after brief 5ms debouncing, showing the resolved overload or an error.

You can find more details on how to use the website in the section [Exploring the paper, interactively](#exploring-the-paper-interactively) below.

## Step by Step Instructions

There are five main commands that you might want to run, all of them are described in more detail below or in other linked Markdown documents.

- `nix run .#website` to run the website server (recommended using instead of running the native binary for exploration and experimentation)
- `nix run .#perf-benchmark` and `nix run .#scale-benchmark` to run benchmarking
- `nix run .#native -- examples/arity.over` to run the CLI, native Rust binary (recommended for manual benchmarking) on an example
- `nix develop` to open a new Nix development shell with all dependencies for development

### Exploring the paper, interactively

We recommend using the website target -- run `nix run .#website`, then check the address, usually `localhost:3000`, then open it in your browser --
to explore example programs, modify them or even write your own programs in Variational Core by using the WASM target.

There are five sections ("boxes") on the website:
- The **editor** on the top left with rudimentary syntax highlighting where examples (both from the paper and outside of it) can be selected, run, and modified. The examples starting with `Section ...`, such as `Section 1.1.1` are direct, commented examples from the paper. Examples starting with `Benchmark: ...` are the performance benchmarks from Section 5.1 of the paper.
- The **output** on the bottom left which is updated on every keystroke, showing the (unsubstituted!) output type, if the overload resolution was a success or a failure (and why),
and the solution(s) in the paper notation.
- The collapsible **Constraints** section on the bottom left showing variational constraints gathered from the source program.
- The collapsible **Bounds** section on the bottom right showing variational bounds solved from the gathered constraints.
- The collapsible **Error Constraints** section on the very bottom showing constraints that can be used to reject worlds and the reason why it is an error constraint.

We also provide a syntax cheat sheet in [SYNTAX_CHEATSHEET.md](./SYNTAX_CHEATSHEET.md) for an easy overview of the supported syntactical constructs.

### Benchmarking

There are two different kinds of benchmarks in the paper:
a performance comparison (`.#perf-benchmark`) and a scalability analysis (`.#scale-benchmark`).

#### Performance Comparison with Swift (Section 5.1)

In order to reproduce Figure 8 in Section 5.1, run the following command which benchmarks our implementation against Swift using Nix:
```bash
$ nix run .#perf-benchmark
```

The benchmark takes just a few minutes to run, its results are then available in `results_perf.{csv, md, json}`. The Markdown version of this file can be used to compare with Figure 8. For comparison, the Markdown table [data/results_perf_default.md](./data/results_perf_default.md) contains the values included the paper measured by the same script. It is OK to stop the script with Ctrl+C, intermediate results are still written to the respective files.

Customization is possible by changing the `perfBenchmarkPackage` derivation in `flake.nix` that just calls the `hyperfine` tool.
More specifically, it's possible to set the minimum number of runs `--min-runs` or `--warmup` , or even to remove/add a new benchmark.
The benchmarked programs are in `./benchmarks/`.

Note that some Swift benchmarks (`uri-*`) fail, this is expected as Swift correctly reports an error.

#### Scalability Analysis (Section 5.2)

The default benchmark can be run using Nix with the following command:
```bash
$ nix run .#scale-benchmark
```
By default, it uses sizes `N = , 4, ..., 16`, exactly as in the rebuttal.
You can run it for any sizes (any `N`s and `M`s) you want with `nix run .#scale-benchmark -- --n-values 5 10 --m-values 10 20`.
You can also use `nix run .#scale-benchmark -- --help` to see all available flags of the underlying `bench-scale.py` script
which uses `hyperfine` to do the actual measuring.

**Warning: Running the benchmark takes about 40 minutes.**
You're free to interrupt it with `Ctrl+C`, you'll still get the (partial output).
The output of the benchmark is located in `results_scale.{csv, md}`. We recommend looking at the Markdown version of the output for a quick overview, disregarding the "baseline" which is nonsensical for our purposes.

Finally, the `bench-scale-plot.py` script is ran in order to produce a plot similar to the one in the paper in Figure 9, saved to in `results_scale.png`.

The CSV version results for the full default run (plain `nix run .#scale-benchmark`) is, as reported in the paper, available in [data/results_scale_default.csv](./data/results_scale_default.csv). Similarly for the plots, available in [data/results_scale_default.png](./data/results_scale_default.png)

## Reusability Guide

Here's how to adapt the artifact for new inputs or new use cases.

### Writing more programs in Variational Core

In order to write more programs in Variational Core, we highly recommend the reader to use the website, described in the section [Exploring the paper, interactively](#exploring-the-paper-interactively) above.

### Writing and running more benchmarks

Currently, our benchmark suite is hardcoded for the one program from the rebuttal and it's nontrivial to update it.
Since we'll be adding more examples as a part of the _Major Revision_, this section will include more relevant details after the revision.

Nevertheless, you can run single programs using `nix run .#native -- $PROGRAM`,
or building the type-checker using `nix build .#native` and then running `result/bin/overloading $PROGRAM`.

Note that the compiler prints both the output and the rough timing:
```
$ result/bin/overloading examples/sec-1-1-1.over

Parsing: 64.375µs
Renaming and gathering choices: 15.583µs
Constraint gathering: 27.5µs
Constraint solving: 21.041µs
Overload resolution: 3.667µs
Getting all solutions: 833ns
Prettifying the output: 21.5µs
Total compilation: 110.916µs
Inferred type: app_1
Constraints:
  (Int, Int) => Int <:^a₁ over_a_0
  (String, String) => String <:^a₂ over_a_0
  over_a_0 <: (Int, Int) => app_1
Bounds:
  Int^a₁, String^a₂ <: app_1 <: ∅
  (Int, Int) => Int^a₁, (String, String) => String^a₂ <: over_a_0 <: (Int, Int) => app_1
Worlds: One
Solutions:
  a₁
```

Building first, and only then running is important for any benchmarking the reader might want to do on their own, as `nix run` _might_ attempt to rebuild.
Our benchmarking suite uses Nix to build once, then run.

### Exploring and adapting the codebase

Please refer to the standard [ARCHITECTURE.md](./ARCHITECTURE.md) document for an explanation of the architecture,
the development, and testing workflows (via `nix develop`), and overview and architectural invariants of each source file.

The codebase itself has a lot of basic unit tests and runs all the files in `examples/`, but the code is itself somewhat sparsely documented,
expecting an experienced reader to have some prior knowledge of how type-checkers look and the paper by their side.

Our main goal is _not_ performance at all costs -- a curious reader might see that we use imperfect data structures (representing type variables as strings, cloning them _everywhere_).
This is done in an effort preserve at least some readability of the underlying algorithm.
