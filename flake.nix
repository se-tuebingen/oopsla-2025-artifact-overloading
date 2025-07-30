{
  description = "Artifact for The Simple Essence of Overloading paper";

  # There are four main commands you might want to run:
  #
  # - `nix run .#native -- examples/rebuttal-mini.over` to run the CLI, native Rust binary (recommended for manual benchmarking) on an example
  # - `nix run .#benchmark` to run the benchmark script
  # - `nix run .#website` to run the website server (recommended to use instead of running the native binary for exploration and experimentation)
  # - `nix develop` to open a new Nix development shell with all dependencies for development

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };

        # Rust toolchain with WASM target
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          targets = [ "wasm32-unknown-unknown" ];
        };

        # Common build inputs
        buildInputs = with pkgs; [
          openssl
        ] ++ lib.optionals stdenv.isDarwin [
          libiconv
        ];

        # Native package
        nativePackage = pkgs.rustPlatform.buildRustPackage {
          pname = "overloading";
          version = "0.1.0";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
          inherit buildInputs;
          nativeBuildInputs = [ pkgs.pkg-config ];
        };

        # WASM package, used in `websiteServer`
        wasmPackage = pkgs.rustPlatform.buildRustPackage {
          pname = "overloading-wasm";
          version = "0.1.0";
          src = ./.;

          nativeBuildInputs = with pkgs; [
            rustToolchain
            wasm-pack
            wasm-bindgen-cli
            binaryen  # Provides `wasm-opt` used by `wasm-pack`
            pkg-config
          ];

          inherit buildInputs;

          cargoLock.lockFile = ./Cargo.lock;

          # Set cache directory to avoid permissions issues
          WASM_PACK_CACHE = ".wasm-pack-cache";

          # Custom build phase using wasm-pack
          buildPhase = ''
            runHook preBuild

            # Build the WASM package
            wasm-pack build --mode no-install --target web --features wasm

            runHook postBuild
          '';

          # Don't use the default cargo build
          dontCargoBuild = true;
          dontCargoCheck = true;

          # Custom install phase to include WASM output (in pkg/), index.html, and examples.json
          installPhase = ''
            runHook preInstall

            mkdir -p $out
            cp -r pkg $out/
            cp examples.json $out/
            cp index.html $out/

            runHook postInstall
          '';
        };

        # Website server using npx serve
        websiteServer = pkgs.writeShellApplication {
          name = "overloading-website";

          runtimeInputs = with pkgs; [
            nodejs
          ];

          text = ''
            WASM_DIR="${wasmPackage}"

            echo "Starting web server for overloading website..."
            echo "WASM package directory: $WASM_DIR"
            echo ""

            cd "$WASM_DIR"
            exec npx serve --no-clipboard
          '';
        };

        # Create a Swift wrapper that sets the proper environment
        swiftWrapper = pkgs.writeShellScriptBin "swift-wrapper" ''
          # Set all required environment variables for Darwin on ARM64
          #
          # They aren't really used for anything since we only use the Swift wrapper to run typechecking,
          # but they are necessary to avoid issues with Swift on Nix on macOS ARM64.
          if [ "$(uname -s)" = "Darwin" ] && [ "$(uname -m)" = "arm64" ]; then
            export NIX_DYNAMIC_LINKER_arm64_apple_darwin=""
            export NIX_CFLAGS_COMPILE_arm64_apple_darwin=""
            export NIX_CFLAGS_COMPILE_BEFORE_arm64_apple_darwin=""
            export NIX_LDFLAGS_arm64_apple_darwin=""
            export NIX_LDFLAGS_BEFORE_arm64_apple_darwin=""
            export NIX_LDFLAGS_AFTER_arm64_apple_darwin=""
            export NIX_CXXSTDLIB_COMPILE_arm64_apple_darwin=""
            export NIX_CXXSTDLIB_LINK_arm64_apple_darwin=""
            export NIX_CFLAGS_LINK_arm64_apple_darwin=""
            export NIX_HARDENING_ENABLE=""
            export NIX_LDFLAGS_HARDEN_arm64_apple_darwin=""
            export NIX_BINTOOLS_WRAPPER_FLAGS_SET_arm64_apple_darwin=""
            export NIX_CC_WRAPPER_FLAGS_SET_arm64_apple_darwin=""

            # Force the target to be compatible with Nix's expectations
            export NIX_CC_TARGET="aarch64-apple-darwin"
          fi

          # Execute Swift with all arguments
          exec ${pkgs.swift}/bin/swiftc "$@"
        '';

        perfBenchmarkPackage = pkgs.writeShellApplication {
          name = "overloading-perf-benchmark";

          runtimeInputs = with pkgs; [
            hyperfine
            swiftWrapper
          ];

          text = ''
            set -e

            echo "Running performance benchmarks..."
            echo "Using Swift wrapper: ${swiftWrapper}/bin/swift-wrapper"
            echo "Using overloading binary: ${nativePackage}/bin/overloading"
            echo ""

            hyperfine -N -i --time-unit millisecond --min-runs 10 --warmup 3 \
              --export-csv results_perf.csv \
              --export-markdown results_perf.md \
              --export-json results_perf.json \
              --command-name "3sat-orig.over" '${nativePackage}/bin/overloading -s benchmarks/3sat-orig.over' \
              --command-name "3sat-orig.swift" '${swiftWrapper}/bin/swift-wrapper -typecheck benchmarks/3sat-orig.swift' \
              --command-name "3sat-hard.over" '${nativePackage}/bin/overloading -s benchmarks/3sat-hard.over' \
              --command-name "3sat-hard.swift" '${swiftWrapper}/bin/swift-wrapper -typecheck benchmarks/3sat-hard.swift' \
              --command-name "uri-orig.over" '${nativePackage}/bin/overloading -s benchmarks/uri-orig.over' \
              --command-name "uri-orig.swift" '${swiftWrapper}/bin/swift-wrapper -typecheck benchmarks/uri-orig.swift' \
              --command-name "uri-big.over" '${nativePackage}/bin/overloading -s benchmarks/uri-big.over' \
              --command-name "uri-big.swift" '${swiftWrapper}/bin/swift-wrapper -typecheck benchmarks/uri-big.swift' \
              --command-name "addneg-orig.over" '${nativePackage}/bin/overloading -s benchmarks/addneg-orig.over' \
              --command-name "addneg-orig.swift" '${swiftWrapper}/bin/swift-wrapper -typecheck benchmarks/addneg-orig.swift' \
              --command-name "addneg-big.over" '${nativePackage}/bin/overloading -s benchmarks/addneg-big.over' \
              --command-name "addneg-big.swift" '${swiftWrapper}/bin/swift-wrapper -typecheck benchmarks/addneg-big.swift' \
              --command-name "recur.over" '${nativePackage}/bin/overloading -s benchmarks/recur.over' \
              --command-name "recur.swift" '${swiftWrapper}/bin/swift-wrapper -typecheck benchmarks/recur.swift' \
              --command-name "dist.over" '${nativePackage}/bin/overloading -s benchmarks/dist.over' \
              --command-name "dist.swift" '${swiftWrapper}/bin/swift-wrapper -typecheck benchmarks/dist.swift' \
              --command-name "cps-10.over" '${nativePackage}/bin/overloading -s benchmarks/cps-10.over' \
              --command-name "cps-10.swift" '${swiftWrapper}/bin/swift-wrapper -typecheck benchmarks/cps-10.swift' \
              --command-name "cps-20.over" '${nativePackage}/bin/overloading -s benchmarks/cps-20.over' \
              --command-name "cps-100.over" '${nativePackage}/bin/overloading -s benchmarks/cps-100.over' \
          ''; # cps-20.swift and cps-100.swift are not benchmarked as they time out...

        };

        scaleBenchmarkPackage = pkgs.writeShellApplication {
          name = "overloading-scale-benchmark";

          runtimeInputs = with pkgs; [
            python3
            hyperfine
            python3Packages.matplotlib
            python3Packages.pandas
            python3Packages.numpy 
          ];

          text = ''
            set -e

            if [ ! -f "bench-scale.py" ]; then
              echo "Error: bench-scale.py not found in current directory"
              echo "Please run this script from the directory containing bench-scale.py"
              exit 1
            fi

            echo "Running parametric benchmarks..."
            echo "Using overloading binary: ${nativePackage}/bin/overloading"
            echo ""

            python3 bench-scale.py \
              --interpreter-cmd "${nativePackage}/bin/overloading {} --short" \
              "$@"

            echo "Drawing a plot..."
            echo ""
            ./bench-scale-plot.py results_scale.csv
          '';
        };

      in {
        packages = {
          default = nativePackage;
          native = nativePackage;
          wasm = wasmPackage;
          website = websiteServer;
          perf-benchmark = perfBenchmarkPackage;
          scale-benchmark = scaleBenchmarkPackage;
          swift-wrapper = swiftWrapper;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = buildInputs ++ [
            rustToolchain

            pkgs.wasm-pack
            pkgs.wasm-bindgen-cli
            pkgs.binaryen # Provides wasm-opt for wasm-pack
            pkgs.rust-analyzer
            pkgs.clippy
            pkgs.rustfmt
            pkgs.cargo-insta

            pkgs.hyperfine
            pkgs.swift
            pkgs.python3
            pkgs.nodejs

            perfBenchmarkPackage
            scaleBenchmarkPackage
            swiftWrapper
            websiteServer
          ];

          nativeBuildInputs = [ pkgs.pkg-config ];
        };

        apps = {
          perf-benchmark = {
            type = "app";
            program = "${perfBenchmarkPackage}/bin/overloading-perf-benchmark";
          };

          scale-benchmark = {
            type = "app";
            program = "${scaleBenchmarkPackage}/bin/overloading-scale-benchmark";
          };

          website = {
            type = "app";
            program = "${websiteServer}/bin/overloading-website";
          };
        };
      });
}
