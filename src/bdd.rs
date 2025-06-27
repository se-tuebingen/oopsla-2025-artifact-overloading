use std::fmt;

use biodivine_lib_bdd::{Bdd, BddVariable, BddVariableSet, BddVariableSetBuilder};
use std::collections::HashMap;

use crate::infer::SolveError;
use crate::syntax::{CVariable, Choices};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ChoiceIndex {
    pub choice_var: CVariable,
    pub alternative: usize, // 0-indexed, but rendered as 1-indexed
}

impl fmt::Display for ChoiceIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}_{}", self.choice_var, self.alternative + 1)
    }
}

impl ChoiceIndex {
    pub fn to_choice(&self) -> (CVariable, usize) {
        (self.choice_var.clone(), self.alternative + 1) // Convert to 1-indexed
    }
}

#[derive(Debug)]
pub struct BddMapping {
    // Bidirectional mapping between choice indices and BDD variables
    choice_to_bdd: HashMap<ChoiceIndex, BddVariable>,
    bdd_to_choice: HashMap<BddVariable, ChoiceIndex>,

    // Group BDD variables by choice variable for easy access
    choice_groups: HashMap<CVariable, Vec<BddVariable>>,
}

impl BddMapping {
    fn new() -> Self {
        Self {
            choice_to_bdd: HashMap::new(),
            bdd_to_choice: HashMap::new(),
            choice_groups: HashMap::new(),
        }
    }

    fn add_mapping(&mut self, choice_idx: ChoiceIndex, bdd_var: BddVariable) {
        self.choice_to_bdd.insert(choice_idx.clone(), bdd_var);
        self.bdd_to_choice.insert(bdd_var, choice_idx.clone());

        // This is a bit of an overkill, we know that we always have all of the choices added at the same time...
        self.choice_groups
            .entry(choice_idx.choice_var.clone())
            .or_default()
            .push(bdd_var);
    }

    pub fn get_bdd_var(&self, choice_idx: &ChoiceIndex) -> Option<BddVariable> {
        self.choice_to_bdd.get(choice_idx).copied()
    }

    pub fn get_choice_index(&self, bdd_var: BddVariable) -> Option<&ChoiceIndex> {
        self.bdd_to_choice.get(&bdd_var)
    }

    pub fn get_choice_group(&self, choice_var: &CVariable) -> Option<&Vec<BddVariable>> {
        self.choice_groups.get(choice_var)
    }

    pub fn all_choice_vars(&self) -> impl Iterator<Item = &CVariable> {
        self.choice_groups.keys()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Worlds {
    No,
    One,
    Many(f64),
}

/// Helper function to build the base BDD for the given choices,
/// ensuring that each dimension has exactly one alternative selected.
fn build_base_bdd(choices: &Choices) -> (Bdd, BddVariableSet, BddMapping) {
    let mut builder = BddVariableSetBuilder::new();
    let mut mapping = BddMapping::new();

    // Build all variables and populate mapping
    for (cvar, max) in choices {
        for i in 0..*max {
            let choice_idx = ChoiceIndex {
                choice_var: cvar.clone(),
                alternative: i,
            };

            // Create BDD variable with a more systematic name
            let bdd_var = builder.make_variable(&choice_idx.to_string());
            mapping.add_mapping(choice_idx, bdd_var);
        }
    }

    let varset = builder.build();

    // Build the main BDD for the expression, starting with `true`
    let mut bdd = varset.mk_true();
    for choice_var in mapping.all_choice_vars() {
        if let Some(choice_vars) = mapping.get_choice_group(choice_var) {
            // Create a BDD constraint that exactly one of the alternatives of a choice is true to ensure mutual exclusivity
            let constraint = varset.mk_sat_exactly_k(1, choice_vars);
            // ... then AND it with the main BDD
            bdd = bdd.and(&constraint);
        }
    }

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
    let (mut bdd, varset, mapping) = build_base_bdd(choices);

    // Build an "incompatibility BDD" representing all incompatibilities, starting `false`
    let mut incompatibility_bdd = varset.mk_false();

    for error in errors {
        let incompatibilities = &error.constraint.choices;

        // Build BDD for this specific incompatibility by ANDing all choice indices
        let mut this_incompatibility = varset.mk_true();

        for (cvar, choice_idx) in incompatibilities {
            let choice_index = ChoiceIndex {
                choice_var: cvar.clone(),
                alternative: (*choice_idx).saturating_sub(1), // Convert 1-indexed to 0-indexed
            };

            if let Some(bdd_var) = mapping.get_bdd_var(&choice_index) {
                let var_bdd = varset.mk_var(bdd_var);
                this_incompatibility = this_incompatibility.and(&var_bdd);
            } else {
                panic!("Choice index {:?} not found in mapping", choice_index);
            }
        }

        // Add this incompatibility to the overall incompatibility BDD using OR
        incompatibility_bdd = incompatibility_bdd.or(&this_incompatibility);
    }

    // Exclude incompatible configurations by ANDing the main BDD with the negation of the incompatibility BDD
    bdd = bdd.and(&incompatibility_bdd.not());

    // let dot = bdd.to_dot_string(&varset, false);
    // println!("BDD Dot representation:\n{}", dot);

    let cardinality = bdd.cardinality();
    let worlds = if cardinality == 0.0 {
        Worlds::No
    } else if cardinality == 1.0 {
        Worlds::One
    } else {
        Worlds::Many(cardinality)
    };

    (bdd, varset, mapping, worlds)
}

/// Given a BDD, a variable set, and a mapping of choice indices to BDD variables,
/// this function returns a vector of all valid solutions represented as `Choices`.
///
/// This can be computationally expensive, as it enumerates all satisfying assignments of the BDD.
pub fn solutions(bdd: &Bdd, varset: &BddVariableSet, mapping: &BddMapping) -> Vec<Choices> {
    let mut solutions = Vec::new();

    bdd.sat_clauses().for_each(|clause| {
        let mut choices = Vec::new();

        for var in varset.variables() {
            if let Some(true) = clause.get_value(var) {
                if let Some(choice_idx) = mapping.get_choice_index(var) {
                    // Convert back to 1-indexed for consistency with Choices
                    choices.push(choice_idx.to_choice());
                }
            }
        }

        solutions.push(choices);
    });

    solutions
}
