use crate::syntax::{Choices, CVariable, Expr};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

pub struct Renamer {
    /// Set of all variable names that are already in use
    used_names: HashSet<String>,
    /// Counter for generating fresh choice variable names
    fresh_counter: usize,
    /// Assumptions
    sugars: HashMap<String, Expr>,
    /// Map of newly generated dimensions to their spans
    dimension_spans: HashMap<crate::syntax::Span, CVariable>,
}

impl Default for Renamer {
    fn default() -> Self {
        Self::new()
    }
}

impl Renamer {
    pub fn new() -> Self {
        Self {
            used_names: HashSet::default(),
            fresh_counter: 0,
            sugars: HashMap::default(),
            dimension_spans: HashMap::default(),
        }
    }

    /// Main entry point to `Renamer`: rename an expression by collecting choices and assigning fresh names
    pub fn rename_and_gather(mut self, expr: Expr) -> Result<(Expr, Choices, HashMap<crate::syntax::Span, CVariable>), String> {
        // 1. collect all existing names to avoid conflicts
        self.collect_all_names(&expr);

        // 2. rename anonymous choice variables and use sugars for assumptions
        let renamed_expr = self.rename_expr(expr);

        // 3. collect all choices from the renamed expression
        let choices = renamed_expr.all_choices()?;

        Ok((renamed_expr, choices, self.dimension_spans))
    }

    /// Collect all variable names (regular variables and choice variables) from the expression
    fn collect_all_names(&mut self, expr: &Expr) {
        match expr {
            Expr::Var(name, _span) => {
                self.used_names.insert(name.clone());
            }
            Expr::Lam(params, body, _span) => {
                for param in params {
                    self.used_names.insert(param.clone());
                }
                self.collect_all_names(body);
            }
            Expr::App(func, args, _span) => {
                self.collect_all_names(func);
                for arg in args {
                    self.collect_all_names(arg);
                }
            }
            Expr::Let(var, binding, body, _span) | Expr::LetRec(var, binding, body, _span) => {
                self.used_names.insert(var.clone());
                self.collect_all_names(binding);
                self.collect_all_names(body);
            }
            Expr::Assume(name, _typ, body, _span) => {
                self.used_names.insert(name.clone());
                self.collect_all_names(body);
            }
            Expr::AssumeOver(name, expr, body, _span) => {
                self.used_names.insert(name.clone());
                self.collect_all_names(expr);
                self.collect_all_names(body);

                // If there's an assumption for this variable, store it
                self.sugars.insert(name.clone(), *expr.clone());
            }
            Expr::If(cond, then_expr, else_expr, _span) => {
                self.collect_all_names(cond);
                self.collect_all_names(then_expr);
                self.collect_all_names(else_expr);
            }
            Expr::Over(choice_var, exprs, _span) => {
                if !choice_var.is_empty() {
                    self.used_names.insert(choice_var.clone());
                }
                for expr in exprs {
                    self.collect_all_names(expr);
                }
            }
            Expr::LitInt(_, _) | Expr::LitBool(_, _) | Expr::LitString(_, _) | Expr::LitDouble(_, _) => {
                // Literals don't contain names
            }
        }
    }

    /// Generate a fresh choice variable name that hasn't been used
    fn generate_fresh_name(&mut self) -> String {
        // Generate names in sequence: a, b, c, ..., z, aa, ab, ac, ...
        loop {
            let name = self.counter_to_name(self.fresh_counter);
            self.fresh_counter += 1;

            if !self.used_names.contains(&name) {
                self.used_names.insert(name.clone());
                return name;
            }
        }
    }

    /// Convert a counter to a name (0 -> "a", 1 -> "b", ..., 25 -> "z", 26 -> "aa", etc.)
    fn counter_to_name(&self, mut counter: usize) -> String {
        let mut result = String::new();

        loop {
            result.push(((counter % 26) as u8 + b'a') as char);
            counter /= 26;
            if counter == 0 {
                break;
            }
            counter -= 1; // Adjust for 0-based indexing
        }

        result.chars().rev().collect()
    }

    fn rename_boxed(&mut self, expr: Expr) -> Box<Expr> {
        Box::new(self.rename_expr(expr))
    }

    /// Rename an expression by replacing anonymous choice variables with fresh names
    fn rename_expr(&mut self, expr: Expr) -> Expr {
        match expr {
            Expr::Var(name, span) => {
                self.sugars
                    .get(&name)
                    .cloned()
                    .map(|assumption| {
                        // If there's an assumption for this variable, use it, but rename it first
                        self.rename_expr(assumption.with_span(span))
                    })
                    .unwrap_or_else(|| Expr::Var(name, span)) // Use assumption if available, otherwise keep the variable
            }

            Expr::Lam(params, body, span) => Expr::Lam(params, Box::new(self.rename_expr(*body)), span),

            Expr::App(func, args, span) => {
                let renamed_func = self.rename_boxed(*func);
                let renamed_args = args.into_iter().map(|arg| self.rename_expr(arg)).collect();
                Expr::App(renamed_func, renamed_args, span)
            }

            Expr::Let(var, binding, body, span) => {
                let renamed_binding = self.rename_boxed(*binding);
                let renamed_body = self.rename_boxed(*body);
                Expr::Let(var, renamed_binding, renamed_body, span)
            }

            Expr::LetRec(var, binding, body, span) => {
                let renamed_binding = self.rename_boxed(*binding);
                let renamed_body = self.rename_boxed(*body);
                Expr::LetRec(var, renamed_binding, renamed_body, span)
            }

            Expr::Assume(name, typ, body, span) => {
                let renamed_body = self.rename_boxed(*body);
                Expr::Assume(name, typ, renamed_body, span)
            }

            Expr::AssumeOver(_, _, body, _) => {
                // NOTE: We just rename the body here, the assumption itself was handled in `collect_all_names`
                // so we don't need to bind anything here.
                // This is a simplification, as the renamer should not have any AssumeOver left
                // after the renaming phase.
                self.rename_expr(*body)
            }

            Expr::If(cond, then_expr, else_expr, span) => {
                let renamed_cond = self.rename_boxed(*cond);
                let renamed_then = self.rename_boxed(*then_expr);
                let renamed_else = self.rename_boxed(*else_expr);
                Expr::If(renamed_cond, renamed_then, renamed_else, span)
            }

            Expr::Over(choice_var, exprs, span) => {
                let renamed_choice_var = if choice_var.is_empty() {
                    let new_choice_var = self.generate_fresh_name();
                    self.dimension_spans.insert(span, new_choice_var.clone());
                    new_choice_var
                } else {
                    choice_var.clone()
                };

                let renamed_exprs = exprs
                    .into_iter()
                    .map(|expr| self.rename_expr(expr))
                    .collect();
                Expr::Over(renamed_choice_var, renamed_exprs, span)
            }

            Expr::LitInt(n, span) => Expr::LitInt(n, span),
            Expr::LitBool(b, span) => Expr::LitBool(b, span),
            Expr::LitString(s, span) => Expr::LitString(s, span),
            Expr::LitDouble(d, span) => Expr::LitDouble(d, span),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::*;

    #[test]
    fn test_rename_anonymous_choice_variables() {
        let renamer = Renamer::new();

        // Test expression: <1, 2> (anonymous choice variable)
        let expr = Expr::Over("".to_string(), vec![Expr::LitInt(1, Span::zero()), Expr::LitInt(2, Span::zero())], Span::zero());

        let (renamed_expr, choices, _) = renamer.rename_and_gather(expr).unwrap();

        // Check that the choice variable got a fresh name
        match renamed_expr {
            Expr::Over(choice_var, _, _) => {
                assert!(!choice_var.is_empty());
                assert_eq!(choice_var, "a"); // First fresh name should be "a"
            }
            _ => panic!("Expected Over expression"),
        }

        // Check that choices were collected correctly
        assert_eq!(choices.len(), 1);
        assert_eq!(choices[0], ("a".to_string(), 2));
    }

    #[test]
    fn test_rename_named_choice_variables() {
        let renamer = Renamer::new();

        // Test expression: x<1, 2> (named choice variable)
        let expr = Expr::Over("x".to_string(), vec![Expr::LitInt(1, Span::zero()), Expr::LitInt(2, Span::zero())], Span::zero());

        let (renamed_expr, choices, _) = renamer.rename_and_gather(expr).unwrap();

        // Check that the choice variable name is preserved
        match renamed_expr {
            Expr::Over(choice_var, _, _) => {
                assert_eq!(choice_var, "x");
            }
            _ => panic!("Expected Over expression"),
        }

        // Check that choices were collected correctly
        assert_eq!(choices.len(), 1);
        assert_eq!(choices[0], ("x".to_string(), 2));
    }

    #[test]
    fn test_rename_multiple_anonymous_choices() {
        let renamer = Renamer::new();

        // Test expression: <1, <2, 3, true>> (nested anonymous choice variables)
        let expr = Expr::Over(
            "".to_string(),
            vec![
                Expr::LitInt(1, Span::zero()),
                Expr::Over(
                    "".to_string(),
                    vec![Expr::LitInt(2, Span::zero()), Expr::LitInt(3, Span::zero()), Expr::LitBool(true, Span::zero())],
                    Span::zero()
                ),
            ],
            Span::zero()
        );

        let (renamed_expr, choices, _) = renamer.rename_and_gather(expr).unwrap();

        // Check that both choice variables got fresh names
        match renamed_expr {
            Expr::Over(outer_choice_var, exprs, _) => {
                assert_eq!(outer_choice_var, "a");

                match &exprs[1] {
                    Expr::Over(inner_choice_var, _, _) => {
                        assert_eq!(inner_choice_var, "b");
                    }
                    _ => panic!("Expected inner Over expression"),
                }
            }
            _ => panic!("Expected outer Over expression"),
        }

        // Check that choices were collected correctly
        assert_eq!(choices.len(), 2);
        assert!(choices.contains(&("a".to_string(), 2)));
        assert!(choices.contains(&("b".to_string(), 3)));
    }

    #[test]
    fn test_avoid_name_conflicts() {
        let renamer = Renamer::new();

        // Test expression: λa => <1, 2> (anonymous choice should not conflict with parameter 'a')
        let expr = Expr::Lam(
            vec!["a".to_string()],
            Box::new(Expr::Over(
                "".to_string(),
                vec![Expr::LitInt(1, Span::zero()), Expr::LitInt(2, Span::zero())],
                Span::zero()
            )),
            Span::zero()
        );

        let (renamed_expr, choices, _) = renamer.rename_and_gather(expr).unwrap();

        // Check that the choice variable got a name different from 'a'
        match renamed_expr {
            Expr::Lam(_, body, _) => {
                match body.as_ref() {
                    Expr::Over(choice_var, _, _) => {
                        assert_ne!(choice_var, "a"); // Should not conflict with parameter
                        assert_eq!(choice_var, "b"); // Should be the next available name
                    }
                    _ => panic!("Expected Over expression in lambda body"),
                }
            }
            _ => panic!("Expected lambda expression"),
        }

        // Check that choices were collected correctly
        assert_eq!(choices.len(), 1);
        assert_eq!(choices[0], ("b".to_string(), 2));
    }

    #[test]
    fn test_counter_to_name() {
        let renamer = Renamer::new();

        assert_eq!(renamer.counter_to_name(0), "a");
        assert_eq!(renamer.counter_to_name(1), "b");
        assert_eq!(renamer.counter_to_name(25), "z");
        assert_eq!(renamer.counter_to_name(26), "aa");
        assert_eq!(renamer.counter_to_name(27), "ab");
        assert_eq!(renamer.counter_to_name(51), "az");
        assert_eq!(renamer.counter_to_name(52), "ba");
    }

    #[test]
    fn test_renamer_assume_expr() {
        let renamer = Renamer::new();

        // Test expression: assume x = 42 in add(x, 1)
        let expr = Expr::AssumeOver(
            "x".to_string(),
            Box::new(Expr::LitInt(42,  Span::zero())),
            Box::new(Expr::App(
                Box::new(Expr::Var("add".to_string(),  Span::zero())),
                vec![Expr::Var("x".to_string(), Span::zero()), Expr::LitInt(1, Span::zero())],
                Span::zero()
            )),
            Span::zero()
        );
        let (renamed_expr, choices, _) = renamer.rename_and_gather(expr).unwrap();
        match renamed_expr {
            Expr::App(func, args, _) => {
                assert_eq!(func.to_string(), "add");
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].to_string(), "42");
                assert_eq!(args[1].to_string(), "1");
            }
            _ => panic!("Expected App expression"),
        }
        // Check that no choices were collected
        assert!(choices.is_empty());
    }

    #[test]
    fn test_renamer_assume_over() {
        let renamer = Renamer::new();

        // Test expression: assume a = <addInt, addStr> in a(a(1, 2), 3)
        let expr = Expr::AssumeOver(
            "a".to_string(),
            Box::new(Expr::Over(
                "".to_string(),
                vec![
                    Expr::Var("addInt".to_string(),  Span::zero()),
                    Expr::Var("addStr".to_string(),  Span::zero()),
                ],
                Span::zero()
            )),
            Box::new(Expr::App(
                Box::new(Expr::Var("a".to_string(),  Span::zero())),
                vec![
                    Expr::App(
                        Box::new(Expr::Var("a".to_string(),  Span::zero())),
                        vec![Expr::LitInt(1,  Span::zero()), Expr::LitInt(2,  Span::zero())],
                        Span::zero()
                    ),
                    Expr::LitInt(3, Span::zero()),
                ],
                Span::zero()
            )),
            Span::zero()
        );

        let (renamed_expr, choices, _) = renamer.rename_and_gather(expr).unwrap();

        assert_eq!(choices.len(), 2);
        match renamed_expr {
            Expr::App(func, args, _) => {
                assert_eq!(func.to_string(), "b‹addInt, addStr›");
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].to_string(), "c‹addInt, addStr›(1, 2)");
                assert_eq!(args[1].to_string(), "3");
            }
            _ => panic!("Expected App expression"),
        }
    }

    #[test]
    fn test_renamer_assume_over_counts() {
        let renamer = Renamer::new();

        // Test expression: assume add = a<addInt, addStr> in add(add(1, 2), 3)
        let expr = Expr::AssumeOver(
            "a".to_string(),
            Box::new(Expr::Over(
                "a".to_string(),
                vec![
                    Expr::Var("addInt".to_string(),  Span::zero()),
                    Expr::Var("addStr".to_string(),  Span::zero()),
                ],
                Span::zero()
            )),
            Box::new(Expr::App(
                Box::new(Expr::Var("a".to_string(),  Span::zero())),
                vec![
                    Expr::App(
                        Box::new(Expr::Var("a".to_string(),  Span::zero())),
                        vec![Expr::LitInt(1,  Span::zero()), Expr::LitInt(2,  Span::zero())],
                        Span::zero()
                    ),
                    Expr::LitInt(3, Span::zero()),
                ],
                Span::zero()
            )),
            Span::zero()
        );

        let (renamed_expr, choices, _) = renamer.rename_and_gather(expr).unwrap();
        assert_eq!(choices.len(), 1);

        // Check that the assumption is preserved
        match renamed_expr {
            Expr::App(func, args, _) => {
                assert_eq!(func.to_string(), "a‹addInt, addStr›");
                assert_eq!(args.len(), 2);
                assert_eq!(args[0].to_string(), "a‹addInt, addStr›(1, 2)");
                assert_eq!(args[1].to_string(), "3");
            }
            _ => panic!("Expected App expression"),
        }
    }
}
