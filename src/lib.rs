use rustc_hash::FxHashMap as HashMap;

#[macro_use]
pub mod syntax;
use crate::json::JsonProcessingResult;
use crate::syntax::{CVariable, Choices, Expr, Span, TVariable, Type, pretty_choices};

pub mod rename;
use crate::rename::Renamer;

pub mod infer;
use crate::infer::{Bounded, Constraint, ConstraintSolver, InferenceContext, SolveError};

pub mod bdd;
use crate::bdd::{Worlds, count_worlds, solutions};

#[cfg(feature = "wasm")]
extern crate console_error_panic_hook;

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

////////////////////////

/// The main runner of the compiler.
/// It processes the expression, gathers constraints, solves them, and performs overload resolution.
fn process_expr(expr: &Expr, get_all_solutions: bool, max_solutions: usize) -> ProcessingResult {
    let (renamed_expr, choices, new_dimension_spans) =
        time_it!("Renaming and gathering choices", {
            let renamer = Renamer::new();
            match renamer.rename_and_gather(expr.clone()) {
                Ok((expr, choices, new_dimension_spans)) => (expr, choices, new_dimension_spans),
                Err(err) => {
                    return ProcessingResult::InferenceError(
                        expr.clone(),
                        format!("Failed to rename: {}", err),
                    );
                }
            }
        });

    let mut ctx = InferenceContext::new();

    let (tpe, constraints) = time_it!("Constraint gathering", {
        match ctx.infer(&renamed_expr) {
            Ok((tpe, constraints)) => (tpe, constraints),
            Err(e) => {
                return ProcessingResult::InferenceError(renamed_expr, e);
            }
        }
    });

    let (bounds, errors) = time_it!("Constraint solving", {
        let solver = ConstraintSolver::new();
        solver.solve_all(constraints.clone())
    });

    let (bdd, var_set, mapping, worlds) =
        time_it!("Overload resolution", { count_worlds(&choices, &errors) });

    let solutions = if get_all_solutions {
        time_it!("Getting all solutions", {
            solutions(&bdd, &var_set, &mapping, max_solutions) // truncate to "only" first 2^16 solutions by default
        })
    } else {
        Vec::new()
    };

    ProcessingResult::AllOK(
        renamed_expr,
        new_dimension_spans,
        tpe,
        constraints,
        bounds,
        errors,
        worlds,
        solutions,
    )
}

pub fn compile_expr(expr: &Expr, short: bool, max_solutions: usize) -> String {
    time_it!("Total compilation", {
        let result = process_expr(expr, true, max_solutions);
        pretty_result(&result, /* print_details */ !short, true, false)
    })
}

/// Represents the result of processing (completely typechecking) an expression.
pub enum ProcessingResult {
    InferenceError(Expr, String),
    AllOK(
        Expr,
        HashMap<Span, CVariable>,
        Type,
        Vec<Constraint>,
        HashMap<TVariable, Bounded>,
        Vec<SolveError>,
        Worlds,
        Vec<Choices>,
    ),
}

impl ProcessingResult {
    fn expr(&self) -> &Expr {
        match self {
            ProcessingResult::InferenceError(expr, _) => expr,
            ProcessingResult::AllOK(expr, _, _, _, _, _, _, _) => expr,
        }
    }
}

/// Pretty-prints the result of processing an expression.
fn pretty_result(
    result: &ProcessingResult,
    print_details: bool,
    print_solutions: bool,
    print_expression: bool,
) -> String {
    time_it!("Prettifying the output", {
        let mut output = String::new();

        if print_expression {
            output.push_str("Expression:\n");
            output.push_str(&result.expr().pretty_print(80));
        }

        match result {
            ProcessingResult::InferenceError(_, e) => {
                output.push_str(&format!("Inference error: {}", e))
            }

            ProcessingResult::AllOK(_, _, tpe, constraints, bounds, _errors, worlds, solutions) => {
                output.push_str(&format!("Inferred type: {}\n", tpe));

                if print_details {
                    output.push_str("Constraints:\n");
                    for c in constraints {
                        output.push_str(&format!("  {}\n", c));
                    }

                    output.push_str("Bounds:\n");

                    // This is just to ensure stable (deterministic) output.
                    let mut tvars: Vec<_> = bounds.keys().collect();
                    tvars.sort_unstable();
                    for tvar in tvars {
                        if let Some(bounded) = bounds.get(tvar) {
                            output.push_str(&bounded.pretty(tvar));
                        } else {
                            panic!(
                                "internal error: Bounded type not found for variable: {}",
                                tvar
                            );
                        }
                    }
                }

                // Note: we explicitly do NOT print the error constraints here.

                output.push_str(&format!("Worlds: {:?}\n", worlds));

                if print_solutions && !solutions.is_empty() {
                    output.push_str("Solutions:\n");
                    if let Worlds::Many(k) = worlds {
                        if solutions.len() < *k as usize {
                            output.push_str(&format!(
                                "  (truncated to first {} solutions, but {} exist in total)\n",
                                solutions.len(),
                                k
                            ));
                        }
                    }
                    for solution in solutions {
                        output.push_str(&format!("  {}\n", pretty_choices(solution)));
                    }
                }
            }
        }

        output
    })
}

/// Submodule for JSON serialization of the compiler results.
pub mod json {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[derive(Serialize, Deserialize, Debug)]
    pub struct JsonConstraint {
        pub from: String,
        pub choices: Vec<String>, // Simplified choice representation
        pub to: String,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct JsonBound {
        pub type_name: String,
        pub choices: Vec<String>,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct JsonBounded {
        pub variable: String,
        pub lower: Vec<JsonBound>,
        pub upper: Vec<JsonBound>,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct JsonError {
        pub constraint: JsonConstraint,
        pub message: String,
    }

    #[derive(Serialize, Deserialize, Debug)]
    #[serde(tag = "type")]
    pub enum JsonWorlds {
        #[serde(rename = "no")]
        No,
        #[serde(rename = "one")]
        One,
        #[serde(rename = "many")]
        Many { count: f64 },
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct JsonDimensionSpan {
        pub span: Span,
        pub variable: CVariable,
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct JsonResult {
        pub expression: String,
        pub dimension_spans: Vec<JsonDimensionSpan>,
        pub inferred_type: String,
        pub worlds: JsonWorlds,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub solutions: Option<Vec<Vec<String>>>, // Each solution as a vector of choice strings
        pub constraints: Vec<JsonConstraint>,
        pub bounds: Vec<JsonBounded>,
        pub errors: Vec<JsonError>,
    }

    #[derive(Serialize, Deserialize, Debug)]
    #[serde(tag = "status")]
    pub enum JsonProcessingResult {
        #[serde(rename = "error")]
        InferenceError { expression: String, error: String },
        #[serde(rename = "success")]
        Success(JsonResult),
    }

    impl From<&Constraint> for JsonConstraint {
        fn from(constraint: &Constraint) -> Self {
            JsonConstraint {
                from: constraint.from.to_string(),
                choices: constraint
                    .choices
                    .iter()
                    .map(|(var, idx)| syntax::pretty_choice(var, *idx))
                    .collect(),
                to: constraint.to.to_string(),
            }
        }
    }

    impl JsonBound {
        fn from_type_choices(tpe: &Type, choices: &Choices) -> Self {
            JsonBound {
                type_name: tpe.to_string(),
                choices: choices
                    .iter()
                    .map(|(var, idx)| syntax::pretty_choice(var, *idx))
                    .collect(),
            }
        }
    }

    impl JsonBounded {
        fn from_bounded(variable: &str, bounded: &Bounded) -> Self {
            // Sort lower and upper bounds for deterministic output
            let mut lower: Vec<_> = bounded
                .lower
                .iter()
                .map(|(tpe, choices)| JsonBound::from_type_choices(tpe, choices))
                .collect();
            lower.sort_by(|a, b| {
                a.type_name
                    .cmp(&b.type_name)
                    .then_with(|| a.choices.cmp(&b.choices))
            });

            let mut upper: Vec<_> = bounded
                .upper
                .iter()
                .map(|(tpe, choices)| JsonBound::from_type_choices(tpe, choices))
                .collect();
            upper.sort_by(|a, b| {
                a.type_name
                    .cmp(&b.type_name)
                    .then_with(|| a.choices.cmp(&b.choices))
            });

            JsonBounded {
                variable: variable.to_string(),
                lower,
                upper,
            }
        }
    }

    impl From<&SolveError> for JsonError {
        fn from(error: &SolveError) -> Self {
            JsonError {
                constraint: JsonConstraint::from(&error.constraint),
                message: error.msg.clone(),
            }
        }
    }

    impl From<&Worlds> for JsonWorlds {
        fn from(worlds: &Worlds) -> Self {
            match worlds {
                Worlds::No => JsonWorlds::No,
                Worlds::One => JsonWorlds::One,
                Worlds::Many(count) => JsonWorlds::Many { count: *count },
            }
        }
    }

    pub fn json_result(result: &ProcessingResult, include_solutions: bool) -> JsonProcessingResult {
        match result {
            ProcessingResult::InferenceError(expr, error) => JsonProcessingResult::InferenceError {
                expression: expr.pretty_print(80),
                error: error.clone(),
            },
            ProcessingResult::AllOK(
                expr,
                dimension_spans,
                tpe,
                constraints,
                bounds,
                errors,
                worlds,
                solutions,
            ) => {
                let json_dimension_spans: Vec<JsonDimensionSpan> = dimension_spans
                    .iter()
                    .map(|(span, var)| JsonDimensionSpan {
                        span: *span,
                        variable: var.clone(),
                    })
                    .collect();

                let json_constraints: Vec<JsonConstraint> =
                    constraints.iter().map(JsonConstraint::from).collect();

                let mut json_bounds: Vec<JsonBounded> = bounds
                    .iter()
                    .map(|(var, bounded)| JsonBounded::from_bounded(var, bounded))
                    .collect();
                // Sort for deterministic output
                json_bounds.sort_by(|a, b| a.variable.cmp(&b.variable));

                let mut json_errors: Vec<JsonError> = errors.iter().map(JsonError::from).collect();
                // Sort errors for deterministic output properly
                json_errors.sort_by(|a, b| {
                    a.constraint
                        .from
                        .cmp(&b.constraint.from)
                        .then_with(|| a.constraint.choices.cmp(&b.constraint.choices))
                        .then_with(|| a.message.cmp(&b.message))
                });

                let json_solutions = if include_solutions && !solutions.is_empty() {
                    Some(
                        solutions
                            .iter()
                            .map(|solution| {
                                solution
                                    .iter()
                                    .map(|(var, idx)| syntax::pretty_choice(var, *idx))
                                    .collect()
                            })
                            .collect(),
                    )
                } else {
                    None
                };

                JsonProcessingResult::Success(JsonResult {
                    expression: expr.pretty_print(80),
                    dimension_spans: json_dimension_spans,
                    inferred_type: tpe.to_string(),
                    constraints: json_constraints,
                    bounds: json_bounds,
                    errors: json_errors,
                    worlds: JsonWorlds::from(worlds),
                    solutions: json_solutions,
                })
            }
        }
    }

    #[derive(Serialize, Deserialize, Debug)]
    pub struct Error {
        pub error: String,
    }

    impl Error {
        pub fn new(message: impl Into<String>) -> Self {
            Error {
                error: message.into(),
            }
        }

        pub fn to_json(&self) -> String {
            serde_json::to_string(self)
                .unwrap_or_else(|_| r#"{"error": "Failed to serialize error message"}"#.to_string())
        }
    }
}

// Convenience function similar to compile_expr
pub fn compile_expr_json_string(
    expr: &Expr,
    include_solutions: bool,
    max_solutions: usize,
) -> String {
    let json = compile_expr_json(expr, include_solutions, max_solutions);
    match serde_json::to_string_pretty(&json) {
        Ok(json) => json,
        Err(e) => make_json_error(format!("Failed to serialize to JSON: {}", e)),
    }
}

pub fn compile_expr_json(
    expr: &Expr,
    include_solutions: bool,
    max_solutions: usize,
) -> JsonProcessingResult {
    let result = process_expr(expr, include_solutions, max_solutions);

    json::json_result(&result, include_solutions)
}

// Convenience function to create error JSON
pub fn make_json_error(message: impl Into<String>) -> String {
    json::Error::new(message).to_json()
}

/// WASM entry point
#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub fn wasm_parse_and_test(input: &str, print_solutions: bool) -> String {
    // I'm not convinced that the panic hook works correctly...
    console_error_panic_hook::set_once();

    match crate::syntax::parse_expr(input) {
        Ok(expr) => {
            std::panic::catch_unwind(|| compile_expr_json_string(&expr, print_solutions, 1 << 10))
                .unwrap_or_else(|_| make_json_error("Panic during compilation".to_string()))
        }
        Err(err) => {
            console_error!("Parse failed: {}", err);
            make_json_error(err.replace("<", "&lt;").replace(">", "&gt;"))
        }
    }
}
