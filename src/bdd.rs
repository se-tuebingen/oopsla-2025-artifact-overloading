use biodivine_lib_bdd::{
    Bdd, BddPartialValuation, BddValuation, BddVariable, BddVariableSet, BddVariableSetBuilder,
};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::infer::SolveError;
use crate::syntax::{CVariable, Choices};

#[derive(Debug, Clone, PartialEq)]
pub enum Worlds {
    No,
    One,
    Many(f64),
}

#[derive(Debug)]
pub struct BddMapping {
    /// Maps each choice variable to its bit variables (logarithmic encoding)
    choice_bit_vars: HashMap<CVariable, Vec<BddVariable>>,
    /// Number of alternatives for each choice variable (for validation)
    choice_sizes: HashMap<CVariable, usize>,
    /// Ordered list of choice variables (for consistent solution ordering)
    choice_order: Vec<CVariable>,
}

impl BddMapping {
    fn new() -> Self {
        Self {
            choice_bit_vars: HashMap::default(),
            choice_sizes: HashMap::default(),
            choice_order: Vec::new(),
        }
    }

    fn add_choice(
        &mut self,
        choice_var: CVariable,
        bit_vars: Vec<BddVariable>,
        num_alternatives: usize,
    ) {
        self.choice_bit_vars.insert(choice_var.clone(), bit_vars);
        self.choice_sizes
            .insert(choice_var.clone(), num_alternatives);
        self.choice_order.push(choice_var);
    }

    pub fn get_bit_vars(&self, choice_var: &CVariable) -> Option<&Vec<BddVariable>> {
        self.choice_bit_vars.get(choice_var)
    }

    pub fn all_choice_vars(&self) -> impl Iterator<Item = &CVariable> {
        self.choice_bit_vars.keys()
    }

    /// Convert a BDD clause (full valuation) to choices, maintaining input order
    pub fn clause_to_choices(&self, clause: &BddValuation) -> Choices {
        let mut choices = Vec::new();

        // Iterate in the same order as the input choices for consistent output
        for choice_var in &self.choice_order {
            if let Some(bit_vars) = self.choice_bit_vars.get(choice_var) {
                let mut binary_value = 0;

                // Compute binary value from bit variables
                for (bit_pos, &bit_var) in bit_vars.iter().enumerate() {
                    if clause.value(bit_var) {
                        binary_value |= 1 << bit_pos
                    }
                }

                if let Some(&num_alternatives) = self.choice_sizes.get(choice_var) {
                    if binary_value < num_alternatives {
                        choices.push((choice_var.clone(), binary_value + 1)); // Convert to 1-indexed
                    } else {
                        panic!(
                            "Binary value {} exceeds number of alternatives for choice variable {:?}",
                            binary_value, choice_var
                        );
                    }
                } else {
                    panic!(
                        "Choice variable {:?} not found in sizes mapping",
                        choice_var
                    );
                }
            }
        }

        choices
    }

    /// Converts "forbidden" choices like `NOT (A = 1 AND B = 2)` into a clause as `A ≠ 1 OR B ≠ 2`.
    /// This is used to build the BDD for incompatibilities.
    ///
    /// Returns `None` if the choices are absurd (e.g., containing A = 1 & A = 2 at the same time),
    /// which indicates that this clause is to be skipped.
    pub fn choices_to_clause(&self, choices: &Choices) -> Option<BddPartialValuation> {
        let mut clause = BddPartialValuation::empty();
        let mut seen: HashSet<&CVariable> = HashSet::default();

        for (cvar, index) in choices {
            // Convert 1-indexed choice value to 0-indexed binary representation
            let alternative = index.saturating_sub(1);

            if !seen.insert(cvar) {
                return None;
            }

            if let Some(bit_vars) = self.choice_bit_vars.get(cvar) {
                // Set the bits in the clause
                for (bit_pos, &bit_var) in bit_vars.iter().enumerate() {
                    let bit_value = (alternative & (1 << bit_pos)) != 0;
                    clause.set_value(bit_var, !bit_value);
                }
            } else {
                panic!("Choice variable {:?} not found in mapping", cvar);
            }
        }

        Some(clause)
    }
}

/// Helper function to build a BDD constraint that ensures a binary value is less than max_value
fn build_range_constraint(
    varset: &BddVariableSet,
    bit_vars: &[BddVariable],
    max_value: usize,
) -> Bdd {
    if max_value == 0 {
        return varset.mk_false();
    }

    if bit_vars.is_empty() {
        return varset.mk_true();
    }

    // If max_value covers all possible bit combinations, no constraint needed
    let max_representable = 1 << bit_vars.len();
    if max_value >= max_representable {
        return varset.mk_true();
    }

    build_less_than_constraint(varset, bit_vars, max_value)
}

/// Build a BDD that is true when the binary value represented by bit_vars is < threshold
fn build_less_than_constraint(
    varset: &BddVariableSet,
    bit_vars: &[BddVariable],
    threshold: usize,
) -> Bdd {
    if threshold == 0 {
        return varset.mk_false();
    }

    if bit_vars.is_empty() {
        return if threshold > 0 {
            varset.mk_true()
        } else {
            varset.mk_false()
        };
    }

    // Recursive approach: value < threshold iff
    // - MSB is 0 and remaining bits < threshold, OR
    // - MSB is 1 and remaining bits < (threshold - 2^(n-1))

    let msb = bit_vars[bit_vars.len() - 1];
    let remaining_bits = &bit_vars[..bit_vars.len() - 1];
    let msb_value = 1 << (bit_vars.len() - 1);

    let msb_false = varset.mk_var(msb).not();
    let msb_true = varset.mk_var(msb);

    if threshold <= msb_value {
        // If threshold <= 2^(n-1), then MSB must be 0
        let remaining_constraint = build_less_than_constraint(varset, remaining_bits, threshold);
        msb_false.and(&remaining_constraint)
    } else {
        // If threshold > 2^(n-1), then either:
        // - MSB = 0 and remaining < threshold, OR
        // - MSB = 1 and remaining < (threshold - 2^(n-1))
        let case1 = msb_false.and(&build_less_than_constraint(
            varset,
            remaining_bits,
            threshold,
        ));
        let case2 = msb_true.and(&build_less_than_constraint(
            varset,
            remaining_bits,
            threshold - msb_value,
        ));
        case1.or(&case2)
    }
}

/// Helper function to build the base BDD for the given choices with logarithmic encoding.
/// It creates bit variables for each choice variable, calculates the necessary range constraints,
/// and combines them into a single BDD.
fn build_base_bdd(choices: &Choices) -> (Bdd, BddVariableSet, BddMapping) {
    let mut builder = BddVariableSetBuilder::new();
    let mut mapping = BddMapping::new();

    // Create bit variables for each choice using logarithmic encoding
    for (cvar, max) in choices {
        // Calculate number of bits needed: ceil(log2(max)), minimum 1
        let num_bits = if *max <= 1 {
            1 // Need at least 1 bit even for single choice
        } else {
            (*max as f64).log2().ceil() as usize
        };

        // Create bit variables with systematic naming
        let bit_vars: Vec<_> = (0..num_bits)
            .map(|bit| builder.make_variable(&format!("{}_bit{}", cvar, bit)))
            .collect();

        mapping.add_choice(cvar.clone(), bit_vars, *max);
    }

    let varset = builder.build();

    // Build range constraints for all choices and combine them efficiently
    let range_constraints: Vec<_> = choices
        .iter()
        .filter_map(|(choice_var, max)| {
            mapping
                .get_bit_vars(choice_var)
                .map(|bit_vars| build_range_constraint(&varset, bit_vars, *max))
        })
        .collect();

    // Combine all range constraints efficiently using fold
    let bdd = range_constraints
        .into_iter()
        .fold(varset.mk_true(), |acc, constraint| acc.and(&constraint));

    (bdd, varset, mapping)
}

/// Taking all of the choices and incompatibilities, build a BDD that represents all valid configurations.
/// The BDD will represent all combinations of choices that do not violate any incompatibilities.
///
/// The function returns the BDD, the variable set, the mapping of choice indices to BDD variables,
/// and the cardinality of the resulting BDD (as a `Worlds` enum).
pub fn count_worlds(
    choices: &Choices,
    errors: &[SolveError],
) -> (Bdd, BddVariableSet, BddMapping, Worlds) {
    // Build the base BDD based on the syntactic structure of the expression for the choices
    let (mut bdd, varset, mapping) = build_base_bdd(choices);

    let mut clauses: Vec<BddPartialValuation> = Vec::with_capacity(errors.len());

    // Turn each incompatibility into a clause (e.g., A != 1 OR B != 2)
    for error in errors {
        if let Some(clause) = mapping.choices_to_clause(&error.constraint.choices) {
            clauses.push(clause);
        }
        // otherwise the clause is absurd (e.g., A = 1 AND A = 2), so we skip it
    }
    // Build a CNF from the clauses
    let forbidden = varset.mk_cnf(&clauses);

    // Combine the base BDD with the forbidden clauses
    bdd = bdd.and(&forbidden);

    let cardinality = bdd.cardinality();
    let worlds = match cardinality {
        0.0 => Worlds::No,
        1.0 => Worlds::One,
        c => Worlds::Many(c),
    };

    (bdd, varset, mapping, worlds)
}

/// Extract all valid solutions from the BDD, maintaining consistent ordering
pub fn solutions(bdd: &Bdd, _varset: &BddVariableSet, mapping: &BddMapping, maximum: usize) -> Vec<Choices> {
    // NOTE: We could also use `sat_clauses()` here for better performance,
    // but then we'd have to handle the partial valuations manually ourselves,
    // since the clauses are only like `a_bit0 := whatever, a_bit1 := true`,
    // which are two solutions in our notation...

    bdd.sat_valuations()
        .take(maximum)
        .map(|clause| mapping.clause_to_choices(&clause))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A simple smoke test for the BDD functionality.
    #[test]
    fn test_bdd_smoke() {
        let choices = vec![
            (CVariable::from("a"), 3), // there was a `a<e1, e2, e3>` somewhere
            (CVariable::from("b"), 2), // there was a `b<e1, e2>` somewhere
        ];
        let (bdd, varset, mapping) = build_base_bdd(&choices);

        assert_eq!(mapping.choice_sizes.get(&CVariable::from("a")), Some(&3));
        assert_eq!(mapping.choice_sizes.get(&CVariable::from("b")), Some(&2));

        // Check that the BDD is not empty
        assert!(!bdd.is_false());
        assert_eq!(bdd.to_string(), "|3,0,0|3,1,1|1,1,0|0,1,2|");

        assert_eq!(varset.variables().len(), 3);
        assert_eq!(varset.to_string(), "[a_bit0,a_bit1,b_bit0]");

        // Check that the mapping works correctly
        let a_bits = mapping.get_bit_vars(&CVariable::from("a")).unwrap();
        assert_eq!(a_bits.len(), 2);
        assert_eq!(a_bits[0].to_string(), "0");
        assert_eq!(a_bits[1].to_string(), "1");

        let b_bits = mapping.get_bit_vars(&CVariable::from("b")).unwrap();
        assert_eq!(b_bits.len(), 1);
        assert_eq!(b_bits[0].to_string(), "2");

        // Variant A: no incompatibilities/errors

        let (bdd2, _, _, worlds2) = count_worlds(&choices, &[]);
        assert_eq!(worlds2, Worlds::Many(6.0));
        assert_eq!(bdd2.to_string(), bdd.to_string());

        // Check that the solutions are correct
        let solutions2 = solutions(&bdd2, &varset, &mapping, 10000);
        assert_eq!(solutions2.len(), 6);
        assert!(solutions2.contains(&vec![(CVariable::from("a"), 1), (CVariable::from("b"), 1)]));
        assert!(solutions2.contains(&vec![(CVariable::from("a"), 1), (CVariable::from("b"), 2)]));
        assert!(solutions2.contains(&vec![(CVariable::from("a"), 2), (CVariable::from("b"), 1)]));
        assert!(solutions2.contains(&vec![(CVariable::from("a"), 2), (CVariable::from("b"), 2)]));
        assert!(solutions2.contains(&vec![(CVariable::from("a"), 3), (CVariable::from("b"), 1)]));
        assert!(solutions2.contains(&vec![(CVariable::from("a"), 3), (CVariable::from("b"), 2)]));

        // Variant B: with incompatibilities/errors
        let errors = vec![
            SolveError {
                constraint: crate::infer::Constraint {
                    from: crate::syntax::Type::TBase("Bool".to_string()),
                    to: crate::syntax::Type::TBase("Int".to_string()),
                    choices: Choices::from(vec![
                        (CVariable::from("a"), 1),
                        (CVariable::from("b"), 2),
                    ]),
                },
                msg: "Incompatibility between a=1 and b=2".to_string(),
            },
            SolveError {
                constraint: crate::infer::Constraint {
                    from: crate::syntax::Type::TBase("Double".to_string()),
                    to: crate::syntax::Type::TBase("String".to_string()),
                    choices: Choices::from(vec![
                        (CVariable::from("a"), 2),
                        (CVariable::from("b"), 1),
                    ]),
                },
                msg: "Incompatibility between a=2 and b=1".to_string(),
            },
        ];
        let (bdd3, _, _, worlds3) = count_worlds(&choices, &errors);

        assert_eq!(worlds3, Worlds::Many(4.0));
        assert_eq!(
            bdd3.to_string(),
            "|3,0,0|3,1,1|2,0,1|1,2,0|2,1,0|1,4,1|0,5,3|"
        );

        // Check that the solutions are correct
        let solutions3 = solutions(&bdd3, &varset, &mapping, 10000);
        assert_eq!(solutions3.len(), 4);
        assert!(solutions3.contains(&vec![(CVariable::from("a"), 1), (CVariable::from("b"), 1)]));
        assert!(solutions3.contains(&vec![(CVariable::from("a"), 3), (CVariable::from("b"), 1)]));
        assert!(solutions3.contains(&vec![(CVariable::from("a"), 3), (CVariable::from("b"), 2)]));
        assert!(solutions3.contains(&vec![(CVariable::from("a"), 2), (CVariable::from("b"), 2)]));

        // Variant C: no solutions
        let errors_no_solutions = vec![
            SolveError {
                constraint: crate::infer::Constraint {
                    from: crate::syntax::Type::TBase("Bool".to_string()),
                    to: crate::syntax::Type::TBase("Int".to_string()),
                    choices: Choices::from(vec![(CVariable::from("a"), 1)]),
                },
                msg: "Incompatibility for a=1".to_string(),
            },
            SolveError {
                constraint: crate::infer::Constraint {
                    from: crate::syntax::Type::TBase("Double".to_string()),
                    to: crate::syntax::Type::TBase("String".to_string()),
                    choices: Choices::from(vec![(CVariable::from("a"), 2)]),
                },
                msg: "Incompatibility for a=2".to_string(),
            },
            SolveError {
                constraint: crate::infer::Constraint {
                    from: crate::syntax::Type::TBase("Bool".to_string()),
                    to: crate::syntax::Type::TBase("Int".to_string()),
                    choices: Choices::from(vec![(CVariable::from("b"), 1)]),
                },
                msg: "Incompatibility for b=1".to_string(),
            },
            SolveError {
                constraint: crate::infer::Constraint {
                    from: crate::syntax::Type::TBase("Double".to_string()),
                    to: crate::syntax::Type::TBase("String".to_string()),
                    choices: Choices::from(vec![(CVariable::from("b"), 2)]),
                },
                msg: "Incompatibility for b=2".to_string(),
            },
        ];

        let (bdd_no_solutions, _, _, worlds_no_solutions) =
            count_worlds(&choices, &errors_no_solutions);
        assert_eq!(worlds_no_solutions, Worlds::No);
        assert!(bdd_no_solutions.is_false());
        assert_eq!(bdd_no_solutions.to_string(), "|3,0,0|");
    }
}
