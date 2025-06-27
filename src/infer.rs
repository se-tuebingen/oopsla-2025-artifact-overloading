use std::collections::{HashMap, HashSet};
use std::fmt;

use crate::syntax::{CVariable, Choices, Expr, TVariable, Type, Variable, meet, pretty_choices};

// Predefined types
fn t_bool() -> Type {
    Type::TBase("Bool".to_string())
}
fn t_int() -> Type {
    Type::TBase("Int".to_string())
}
fn t_string() -> Type {
    Type::TBase("String".to_string())
}
fn t_double() -> Type {
    Type::TBase("Double".to_string())
}

// Constraint type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constraint {
    pub from: Type,
    pub choices: Choices,
    pub to: Type,
}

impl Constraint {
    fn new(from: Type, choices: Choices, to: Type) -> Self {
        Constraint { from, choices, to }
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.choices.is_empty() {
            write!(f, "{} <: {}", self.from, self.to)
        } else {
            write!(
                f,
                "{} <:^{} {}",
                self.from,
                pretty_choices(&self.choices),
                self.to
            )
        }
    }
}

// Bounded type for constraint solving
#[derive(Debug, Clone)]
pub struct Bounded {
    pub lower: HashSet<(Type, Choices)>,
    pub upper: HashSet<(Type, Choices)>,
}

fn format_bounds(bounds: &HashSet<(Type, Choices)>) -> String {
    if bounds.is_empty() {
        "∅".to_string()
    } else {
        // This is just to ensure stable (deterministic) output.
        let mut sorted_bounds: Vec<_> = bounds.iter().collect();
        sorted_bounds.sort_by(|(a_type, a_choices), (b_type, b_choices)| {
            a_type
                .to_string()
                .cmp(&b_type.to_string())
                .then_with(|| a_choices.cmp(b_choices))
        });

        sorted_bounds
            .iter()
            .map(|(ty, choices)| {
                if choices.is_empty() {
                    ty.to_string()
                } else {
                    format!("{}^{}", ty, pretty_choices(choices))
                }
            })
            .collect::<Vec<_>>()
            .join(", ")
    }
}

impl Bounded {
    fn empty() -> Self {
        Bounded {
            lower: HashSet::new(),
            upper: HashSet::new(),
        }
    }

    pub fn pretty(&self, tvar: &str) -> String {
        format!(
            "  {} <: {} <: {}\n",
            format_bounds(&self.lower),
            tvar,
            format_bounds(&self.upper)
        )
    }
}

#[derive(Debug)]
pub struct SolveError {
    pub constraint: Constraint,
    pub msg: String,
}

impl fmt::Display for SolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} in {}", self.msg, self.constraint)
    }
}

// Type inference context
pub struct InferenceContext {
    var_counter: usize,
    type_env: Vec<HashMap<Variable, Type>>,
    choice_env: Vec<HashMap<CVariable, usize>>,
}

impl Default for InferenceContext {
    fn default() -> Self {
        InferenceContext::new()
    }
}

impl InferenceContext {
    pub fn new() -> Self {
        InferenceContext {
            var_counter: 0,
            type_env: vec![HashMap::new()],
            choice_env: vec![HashMap::new()],
        }
    }

    fn fresh(&mut self, hint: &str) -> Type {
        let var = format!("{}_{}", hint, self.var_counter);
        self.var_counter += 1;
        Type::TVar(var)
    }

    fn lookup(&mut self, x: &Variable) -> Result<Type, String> {
        for env in self.type_env.iter().rev() {
            if let Some(tpe) = env.get(x) {
                match tpe {
                    Type::TScheme(vars, tpe) => {
                        // Create fresh type variables for each bound variable.
                        // Doesn't use `self.fresh` directly because of borrowing issues.
                        let fresh_vars: Vec<(TVariable, Type)> = vars
                            .iter()
                            .map(|v| {
                                let fresh_var = format!("{}_{}", v, self.var_counter);
                                self.var_counter += 1;
                                (v.clone(), Type::TVar(fresh_var))
                            })
                            .collect();

                        let substitution: HashMap<TVariable, Type> =
                            fresh_vars.into_iter().collect();

                        // Apply the substitution to instantiate the type scheme
                        return Ok(tpe.substitute(&substitution));
                    }
                    _ => return Ok(tpe.clone()),
                }
            }
        }
        Err(format!(
            "Variable {} not found.\nDid you mean to assume an extern, e.g. `assume {}: (Int, Double) => String;`?",
            x, x
        ))
    }

    fn push_env(&mut self) {
        self.type_env.push(HashMap::new());
        self.choice_env.push(HashMap::new());
    }

    fn pop_env(&mut self) {
        self.type_env.pop();
        self.choice_env.pop();
    }

    pub fn bind_var(&mut self, x: Variable, tpe: Type) {
        if let Some(env) = self.type_env.last_mut() {
            env.insert(x, tpe);
        }
    }

    fn bind_choice(&mut self, x: CVariable, n: usize) {
        if let Some(env) = self.choice_env.last_mut() {
            env.insert(x, n);
        }
    }

    fn current_choices(&self) -> Choices {
        let mut choices = Vec::new();
        for env in &self.choice_env {
            for (var, num) in env {
                choices.push((var.clone(), *num));
            }
        }
        choices
    }

    pub fn infer(&mut self, e: &Expr) -> Result<(Type, Vec<Constraint>), String> {
        let mut constraints = Vec::new();

        let constrain =
            |constraints: &mut Vec<Constraint>, from: Type, to: Type, choices: Choices| {
                constraints.push(Constraint::new(from, choices, to));
            };

        let result_type = match e {
            Expr::Var(x) => self.lookup(x)?,

            Expr::Lam(xs, body) => {
                self.push_env();
                let mut arg_types = Vec::new();

                for x in xs {
                    let tpe = self.fresh(x);
                    self.bind_var(x.clone(), tpe.clone());
                    arg_types.push(tpe);
                }

                let (return_type, mut body_constraints) = self.infer(body)?;
                constraints.append(&mut body_constraints);

                self.pop_env();
                Type::TFun(arg_types, Box::new(return_type))
            }

            Expr::App(f, args) => {
                let (callee_type, mut f_constraints) = self.infer(f)?;
                constraints.append(&mut f_constraints);

                let mut arg_types = Vec::new();
                for arg in args {
                    let (arg_type, mut arg_constraints) = self.infer(arg)?;
                    constraints.append(&mut arg_constraints);
                    arg_types.push(arg_type);
                }

                let result_type = self.fresh("app");
                constrain(
                    &mut constraints,
                    callee_type,
                    Type::TFun(arg_types, Box::new(result_type.clone())),
                    self.current_choices(),
                );

                result_type
            }

            Expr::Let(x, binding, body) => {
                let (binding_type, mut binding_constraints) = self.infer(binding)?;
                constraints.append(&mut binding_constraints);

                self.push_env();
                self.bind_var(x.clone(), binding_type);
                let (body_type, mut body_constraints) = self.infer(body)?;
                constraints.append(&mut body_constraints);
                self.pop_env();

                body_type
            }

            Expr::LetRec(x, binding, body) => {
                let tpe = self.fresh("rec");

                self.push_env();
                self.bind_var(x.clone(), tpe.clone());
                let (binding_type, mut binding_constraints) = self.infer(binding)?;
                constraints.append(&mut binding_constraints);
                self.pop_env();

                constrain(
                    &mut constraints,
                    binding_type,
                    tpe.clone(),
                    self.current_choices(),
                );

                self.push_env();
                self.bind_var(x.clone(), tpe);
                let (body_type, mut body_constraints) = self.infer(body)?;
                constraints.append(&mut body_constraints);
                self.pop_env();

                body_type
            }

            Expr::Assume(x, typ, body) => {
                self.bind_var(x.clone(), typ.clone());
                self.push_env();
                let (body_type, mut body_constraints) = self.infer(body)?;
                constraints.append(&mut body_constraints);
                self.pop_env();

                body_type
            }

            Expr::If(cond, thn, els) => {
                let result_type = self.fresh("if");

                let (cond_type, mut cond_constraints) = self.infer(cond)?;
                constraints.append(&mut cond_constraints);

                let (then_type, mut then_constraints) = self.infer(thn)?;
                constraints.append(&mut then_constraints);

                let (else_type, mut else_constraints) = self.infer(els)?;
                constraints.append(&mut else_constraints);

                let choices = self.current_choices();
                constrain(&mut constraints, cond_type, t_bool(), choices.clone());
                constrain(
                    &mut constraints,
                    then_type,
                    result_type.clone(),
                    choices.clone(),
                );
                constrain(&mut constraints, else_type, result_type.clone(), choices);

                result_type
            }

            Expr::Over(choice_var, exprs) => {
                let result_type = self.fresh(&format!("over_{}", choice_var));

                for (i, expr) in exprs.iter().enumerate() {
                    self.push_env();
                    self.bind_choice(choice_var.clone(), i + 1);

                    let (expr_type, mut expr_constraints) = self.infer(expr)?;
                    constraints.append(&mut expr_constraints);

                    constrain(
                        &mut constraints,
                        expr_type,
                        result_type.clone(),
                        self.current_choices(),
                    );
                    self.pop_env();
                }

                result_type
            }

            Expr::LitInt(_) => t_int(),
            Expr::LitBool(_) => t_bool(),
            Expr::LitString(_) => t_string(),
            Expr::LitDouble(_) => t_double(),
        };

        Ok((result_type, constraints))
    }
}

// Constraint solver
pub struct ConstraintSolver {
    seen: HashSet<Constraint>,
    bounds: HashMap<TVariable, Bounded>,
}

impl Default for ConstraintSolver {
    fn default() -> Self {
        ConstraintSolver::new()
    }
}

impl ConstraintSolver {
    pub fn new() -> Self {
        ConstraintSolver {
            seen: HashSet::new(),
            bounds: HashMap::new(),
        }
    }

    fn get_bounds_of(&mut self, tvar: &TVariable) -> &Bounded {
        self.bounds
            .entry(tvar.clone())
            .or_insert_with(Bounded::empty)
    }

    fn add_lower(&mut self, tvar: TVariable, tpe: Type, choices: Choices) {
        let entry = self.bounds.entry(tvar).or_insert_with(Bounded::empty);
        entry.lower.insert((tpe, choices));
    }

    fn add_upper(&mut self, tvar: TVariable, tpe: Type, choices: Choices) {
        let entry = self.bounds.entry(tvar).or_insert_with(Bounded::empty);
        entry.upper.insert((tpe, choices));
    }

    /// Main function for resolving a single constraint.
    /// It both returns new constraints to be processed and may return an error if the given constraint is unsolvable.
    #[allow(clippy::result_large_err)]
    fn solve_constraint(&mut self, constraint: &Constraint) -> Result<Vec<Constraint>, SolveError> {
        if self.seen.contains(constraint) {
            return Ok(Vec::new());
        }

        self.seen.insert(constraint.clone());
        let mut new_constraints = Vec::new();

        match (&constraint.from, &constraint.to) {
            (Type::TBase(left), Type::TBase(right)) => {
                if left != right {
                    return Err(SolveError {
                        constraint: constraint.clone(),
                        msg: "Incompatible base types".to_string(),
                    });
                }
            }

            // "Sub" from the paper for function types
            //
            // (α => β) <: (γ => δ)
            //          ~>
            // γ <: α (contravariant!)
            // β <: δ (covariant!)
            (Type::TFun(lps, lres), Type::TFun(ups, ures)) => {
                if lps.len() != ups.len() {
                    return Err(SolveError {
                        constraint: constraint.clone(),
                        msg: "Wrong arity".to_string(),
                    });
                }

                // Argument types are contravariant
                for (l, u) in lps.iter().zip(ups.iter()) {
                    new_constraints.push(Constraint::new(
                        u.clone(),
                        constraint.choices.clone(),
                        l.clone(),
                    ));
                }

                // Return type constraint (covariant)
                new_constraints.push(Constraint::new(
                    (**lres).clone(),
                    constraint.choices.clone(),
                    (**ures).clone(),
                ));
            }

            // Rule "UpperBound": we found that x <: u, that is `x` has an upper bound `u`.
            (Type::TVar(x), u) => {
                // 1. For each lower bound `t` of `x`, we add a new constraint `t <: u` to ensure consistency.
                let bounded = self.get_bounds_of(x).clone();
                for (tpe, c) in bounded.lower {
                    new_constraints.push(Constraint::new(
                        tpe,
                        meet(&constraint.choices, &c),
                        u.clone(),
                    ));
                }

                // 2. We add the upper bound `u` for `x` itself.
                self.add_upper(x.clone(), u.clone(), constraint.choices.clone());
            }

            // Rule "LowerBound": we found that l <: y, that is `l` has a lower bound `y`.
            (l, Type::TVar(y)) => {
                // 1. For each upper bound `t` of `y`, we add a new constraint `l <: t` to ensure consistency.
                let bounded = self.get_bounds_of(y).clone();
                for (tpe, c) in bounded.upper {
                    new_constraints.push(Constraint::new(
                        l.clone(),
                        meet(&constraint.choices, &c),
                        tpe,
                    ));
                }

                // 2. We add the lower bound `l` for `y` itself.
                self.add_lower(y.clone(), l.clone(), constraint.choices.clone());
            }

            _ => {
                return Err(SolveError {
                    constraint: constraint.clone(),
                    msg: "Unsolvable constraint (fallthrough)".to_string(),
                });
            }
        }

        Ok(new_constraints)
    }

    /// Main function to solve all constraints in a given set.
    /// It processes each constraint iteratively, collecting new constraints and errors,
    /// accumulating and eventually returning both.
    pub fn solve_all(
        mut self,
        initial_constraints: Vec<Constraint>,
    ) -> (HashMap<TVariable, Bounded>, Vec<SolveError>) {
        let mut queue = initial_constraints;
        let mut errors = Vec::new();

        while let Some(constraint) = queue.pop() {
            match self.solve_constraint(&constraint) {
                Ok(mut new_constraints) => {
                    queue.append(&mut new_constraints);
                }
                Err(error) => {
                    errors.push(error);
                }
            }
        }

        (self.bounds.clone(), errors)
    }
}

#[cfg(test)]
mod subst_tests {
    use super::*;

    #[test]
    fn test_substitution_with_your_types() {
        // Test simple variable substitution
        let mut subst = HashMap::new();
        subst.insert("a".to_string(), Type::TBase("Int".to_string()));

        let original = Type::TVar("a".to_string());
        let result = original.substitute(&subst);
        assert_eq!(result, Type::TBase("Int".to_string()));

        // Test function type substitution with multiple arguments
        let fun_type = Type::TFun(
            vec![Type::TVar("a".to_string()), Type::TVar("b".to_string())],
            Box::new(Type::TVar("c".to_string())),
        );

        subst.insert("b".to_string(), Type::TBase("Bool".to_string()));
        subst.insert("c".to_string(), Type::TBase("String".to_string()));

        let result = fun_type.substitute(&subst);

        if let Type::TFun(args, ret) = result {
            assert_eq!(args[0], Type::TBase("Int".to_string()));
            assert_eq!(args[1], Type::TBase("Bool".to_string()));
            assert_eq!(*ret, Type::TBase("String".to_string()));
        } else {
            panic!("Expected function type");
        }
    }

    #[test]
    fn test_type_scheme_instantiation_fixed() {
        let mut ctx = InferenceContext::new();

        // Create a polymorphic function: forall a b. a -> b -> a
        let poly_scheme = Type::TScheme(
            vec!["a".to_string(), "b".to_string()],
            Box::new(Type::TFun(
                vec![Type::TVar("a".to_string())],
                Box::new(Type::TFun(
                    vec![Type::TVar("b".to_string())],
                    Box::new(Type::TVar("a".to_string())),
                )),
            )),
        );

        // Add it to the environment
        ctx.type_env[0].insert("const".to_string(), poly_scheme);

        // Look it up - should get a fresh instantiation each time
        let result1 = ctx.lookup(&"const".to_string()).unwrap();
        let result2 = ctx.lookup(&"const".to_string()).unwrap();

        // println!("First instantiation: {:?}", result1);
        // println!("Second instantiation: {:?}", result2);

        // Both should be function types, but with different type variables
        assert_ne!(result1, result2);
        assert!(matches!(result1, Type::TFun(_, _)));
        assert!(matches!(result2, Type::TFun(_, _)));
    }

    #[test]
    fn test_type_scheme_instantiation() {
        let mut ctx = InferenceContext::new();

        let id1_type = Type::TScheme(
            vec!["a".to_string()],
            Box::new(Type::TFun(
                vec![Type::TVar("a".to_string())],
                Box::new(Type::TVar("a".to_string())),
            )),
        );
        ctx.type_env[0].insert("id1".to_string(), id1_type);

        let id2_type = Type::TScheme(
            vec!["a".to_string()],
            Box::new(Type::TFun(
                vec![Type::TVar("a".to_string())],
                Box::new(Type::TVar("a".to_string())),
            )),
        );
        ctx.type_env[0].insert("id2".to_string(), id2_type);

        let inst1 = ctx.lookup(&"id1".to_string()).unwrap();
        let inst2 = ctx.lookup(&"id2".to_string()).unwrap();
        // println!("First instantiation: {:?}", inst1);
        // println!("Second instantiation: {:?}", inst2);

        // Both should be function types, but with different type variables
        assert!(matches!(inst1, Type::TFun(_, _)));
        assert!(matches!(inst2, Type::TFun(_, _)));
        if let (Type::TFun(args1, ret1), Type::TFun(args2, ret2)) = (inst1, inst2) {
            assert_eq!(args1.len(), 1);
            assert_eq!(args2.len(), 1);

            // id1 is instantiated correctly
            assert_eq!(args1[0], *ret1);
            // id2 is instantiated correctly
            assert_eq!(args2[0], *ret2);

            // But they are instantiated with different type variables
            assert_ne!(args1[0], args2[0]);
            assert_ne!(*ret1, *ret2);
        } else {
            panic!("Expected function types");
        }
    }
}

#[cfg(test)]
mod infer_tests {
    use super::*;

    /// Simple smoke test for super basic type inference of non-generalized `let`, _excluding_ choices.
    #[test]
    fn test_infer_smoke() {
        use crate::syntax::*;
        let mut ctx = InferenceContext::new();

        let expr: Expr = Expr::Let(
            "id".to_string(),
            Box::new(Expr::Lam(
                vec!["x".to_string()],
                Box::new(Expr::Var("x".to_string())),
            )),
            Box::new(Expr::App(
                Box::new(Expr::Var("id".to_string())),
                vec![Expr::LitInt(42)],
            )),
        );

        let (result_type, constraints) = ctx.infer(&expr).expect("Inference failed");
        // println!("Result type: {:?}", result_type);
        // println!("Constraints: {:?}", constraints);
        assert_eq!(result_type, Type::TVar("app_1".to_string()));
        assert_eq!(constraints.len(), 1);
        if let Type::TFun(args, ret) = &constraints[0].from {
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], Type::TVar("x_0".to_string()));
            assert_eq!(**ret, Type::TVar("x_0".to_string()));
        } else {
            panic!("Expected function type in constraints");
        }
        assert_eq!(
            constraints[0].to,
            Type::TFun(
                vec![Type::TBase("Int".to_string())],
                Box::new(Type::TVar("app_1".to_string()))
            )
        );

        // Check that the choices are empty
        assert!(constraints[0].choices.is_empty(), "Expected empty choices");

        // Check that the constraint solver steps
        let solver = ConstraintSolver::new();
        let (bounds, errors) = solver.solve_all(constraints);
        // println!("Bounds: {:?}", bounds);
        // println!("Errors: {:?}", errors);
        assert!(errors.is_empty(), "Expected no errors during solving");

        // Check that the bounds for the variable 'app_1' are correct
        // Int <: app_1 <: ∅
        let bounded = bounds.get("app_1").expect("Expected bounds for app_1");
        assert!(
            !bounded.lower.is_empty(),
            "Expected lower bounds to be non-empty"
        );
        assert!(
            bounded.upper.is_empty(),
            "Expected upper bounds to be empty"
        );
        assert!(
            bounded
                .lower
                .iter()
                .any(|(t, _)| *t == Type::TBase("Int".to_string())),
            "Expected lower bounds to contain Int"
        );

        // Check that the bounds for the variable 'x_0' are correct
        // Int <: x_0 <: app_1
        let bounded_x0 = bounds.get("x_0").expect("Expected bounds for x_0");
        assert!(
            !bounded_x0.lower.is_empty(),
            "Expected lower bounds for x_0 to be non-empty"
        );
        assert!(
            !bounded_x0.upper.is_empty(),
            "Expected upper bounds for x_0 to be non-empty"
        );
        assert!(
            bounded_x0
                .lower
                .iter()
                .any(|(t, _)| *t == Type::TBase("Int".to_string())),
            "Expected lower bounds for x_0 to contain Int"
        );
        assert!(
            bounded_x0
                .upper
                .iter()
                .any(|(t, _)| *t == Type::TVar("app_1".to_string())),
            "Expected upper bounds for x_0 to contain app_1"
        );
    }
}
