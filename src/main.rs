use overloading::compile_expr;
use overloading::syntax::parse_expr;

use clap::Parser;

#[derive(Parser)]
#[command(name = "overloading")]
struct Args {
    /// Input file to process
    filename: String,

    /// Use short output format
    #[arg(short, long)]
    short: bool,
}

/// Main entry point for the native CLI.
fn main() {
    let args = Args::parse();

    let content = std::fs::read_to_string(&args.filename).expect("Failed to read file");
    match parse_expr(&content) {
        Ok(expr) => {
            println!("{}", compile_expr(&expr, args.short));
        }
        Err(err) => {
            panic!("Failed to parse expression: {}", err);
        }
    }
}

/// Snapshot tests for the compiler examples.
#[cfg(test)]
#[test]
fn test_compiler_examples() {
    use insta::{assert_json_snapshot, glob, with_settings};

    glob!("../", "examples/*.over", |path| {
        let input = std::fs::read_to_string(path).unwrap();
        let output = match parse_expr(&input) {
            Ok(expr) => overloading::compile_expr_json(&expr, true),
            Err(err) => overloading::json::JsonProcessingResult::InferenceError {
                expression: input,
                error: err,
            },
        };

        // Use the filename (without extension) as snapshot suffix
        let filename = path.file_stem().unwrap().to_str().unwrap();
        with_settings!({
            snapshot_path => "../examples/snapshots",
            snapshot_suffix => filename,
            sort_maps => true,
        }, {
            assert_json_snapshot!(output);
        });
    });
}
