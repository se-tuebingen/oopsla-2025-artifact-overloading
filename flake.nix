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

        # Benchmark script package
        benchmarkPackage = pkgs.writeShellApplication {
          name = "overloading-benchmark";

          runtimeInputs = with pkgs; [
            python3
            hyperfine
            swiftWrapper
          ];

          text = ''
            set -e

            if [ ! -f "bench.py" ]; then
              echo "Error: bench.py not found in current directory"
              echo "Please run this script from the directory containing bench.py"
              exit 1
            fi

            echo "Running overloading benchmarks..."
            echo "Using Swift wrapper: ${swiftWrapper}/bin/swift-wrapper"
            echo "Using overloading binary: ${nativePackage}/bin/overloading"
            echo ""

            python3 bench.py \
              --over-cmd "${nativePackage}/bin/overloading {} --short" \
              --swift-cmd "${swiftWrapper}/bin/swift-wrapper -typecheck {}" \
              "$@"
          '';
        };

      in {
        packages = {
          default = nativePackage;
          native = nativePackage;
          wasm = wasmPackage;
          website = websiteServer;
          benchmark = benchmarkPackage;
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

            benchmarkPackage
            swiftWrapper
            websiteServer
          ];

          nativeBuildInputs = [ pkgs.pkg-config ];
        };

        apps = {
          benchmark = {
            type = "app";
            program = "${benchmarkPackage}/bin/overloading-benchmark";
          };

          website = {
            type = "app";
            program = "${websiteServer}/bin/overloading-website";
          };
        };
      });
}
