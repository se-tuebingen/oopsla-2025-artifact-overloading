// Timing macros live here, because I can't figure out how to import them correctly from other modules.

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

#[cfg(feature = "wasm")]
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);
    #[wasm_bindgen(js_namespace = console)]
    pub fn error(s: &str);
}

#[cfg(feature = "wasm")]
macro_rules! console_log {
    ($($t:tt)*) => (crate::syntax::log(&format_args!($($t)*).to_string()))
}

#[cfg(feature = "wasm")]
macro_rules! console_error {
    ($($t:tt)*) => (crate::syntax::error(&format_args!($($t)*).to_string()))
}

// Non-WASM version of `time_it` that shows timing information
#[cfg(all(not(feature = "wasm"), not(test)))]
macro_rules! time_it {
    ($name:expr, $code:block) => {{
        let start = std::time::Instant::now();
        let result = $code;
        let duration = start.elapsed();
        println!("{}: {:?}", $name, duration);
        result
    }};
}

// WASM version of `time_it` that doesn't actually time anything
// Also used during tests to avoid timing noise
#[cfg(any(feature = "wasm", test))]
macro_rules! time_it {
    ($name:expr, $code:block) => {{
        let result = $code;
        result
    }};
}

///////////////////////////////////////////////////////////////

// Position and span information
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub struct Position {
    pub offset: usize,
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn new(offset: usize, line: u32, column: u32) -> Self {
        Self {
            offset,
            line,
            column,
        }
    }

    pub fn zero() -> Self {
        Self {
            offset: 0,
            line: 1,
            column: 1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn zero() -> Self {
        Self {
            start: Position::zero(),
            end: Position::zero(),
        }
    }

    pub fn extend(&self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start.line == self.end.line {
            write!(
                f,
                "{}:{}-{}",
                self.start.line, self.start.column, self.end.column
            )
        } else {
            write!(f, "{}-{}", self.start, self.end)
        }
    }
}

// Type aliases
pub type CVariable = String;
pub type Variable = String;
pub type TVariable = String;

// Choices type with spans
pub type Choices = Vec<(CVariable, usize)>;

// Computes the meet of two sets of choices.
// See tests at the end of the file for examples of how this works.
pub fn meet(c1: &Choices, c2: &Choices) -> Choices {
    let mut result = c1.clone();
    result.extend(c2.iter().cloned());
    nub(result)
}

fn nub(mut list: Choices) -> Choices {
    let mut seen = HashSet::default();
    list.retain(|(var, num)| seen.insert((var.clone(), *num)));
    list
}

#[cfg(test)]
mod choices_tests {
    use super::*;

    #[test]
    fn test_nub_different() {
        let mut choices = vec![
            ("a".to_string(), 1),
            ("b".to_string(), 2),
            ("a".to_string(), 1),
            ("c".to_string(), 3),
        ];
        choices = nub(choices);
        assert_eq!(choices.len(), 3);
        assert!(choices.iter().any(|(var, num)| var == "a" && *num == 1));
        assert!(choices.iter().any(|(var, num)| var == "b" && *num == 2));
        assert!(choices.iter().any(|(var, num)| var == "c" && *num == 3));
    }

    #[test]
    fn test_nub_same() {
        let mut choices = vec![("a".to_string(), 1), ("a".to_string(), 2)];
        choices = nub(choices);
        assert_eq!(choices.len(), 2);
        assert!(choices.iter().any(|(var, num)| var == "a" && *num == 1));
        assert!(choices.iter().any(|(var, num)| var == "a" && *num == 2));
    }

    #[test]
    fn test_meet() {
        let c1 = vec![("a".to_string(), 1), ("b".to_string(), 2)];
        let c2 = vec![("b".to_string(), 2), ("c".to_string(), 3)];
        let result = meet(&c1, &c2);
        assert_eq!(result.len(), 3);
        assert!(result.iter().any(|(var, num)| var == "a" && *num == 1));
        assert!(result.iter().any(|(var, num)| var == "b" && *num == 2));
        assert!(result.iter().any(|(var, num)| var == "c" && *num == 3));
    }
}

pub fn pretty_choice(cvar: &CVariable, n: usize) -> String {
    const SUBSCRIPTS: [char; 10] = ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'];

    let subscript = {
        if n == 0 {
            SUBSCRIPTS[0].to_string()
        } else {
            let mut result = String::new();
            let mut current = n;
            let mut digits = Vec::new();

            while current > 0 {
                digits.push(SUBSCRIPTS[current % 10]);
                current /= 10;
            }

            for digit in digits.iter().rev() {
                result.push(*digit);
            }
            result
        }
    };

    format!("{}{}", cvar, subscript)
}

pub fn pretty_choices(c: &Choices) -> String {
    c.iter()
        .map(|(cvar, n)| pretty_choice(cvar, *n))
        .collect::<Vec<_>>()
        .join("")
}

pub fn parse_expr(input: &str) -> Result<Expr, String> {
    time_it!("Parsing", {
        match parser::expr_parser(input.trim()) {
            Ok(("", expr)) => Ok(expr),
            Ok((remaining, _)) => Err(format!("Unexpected input: {}", remaining)),
            Err(e) => Err(format!("{}", e)),
        }
    })
}

// Expression AST with spans
#[derive(Debug, Clone)]
pub enum Expr {
    Var(Variable, Span),
    Lam(Vec<Variable>, Box<Expr>, Span),
    App(Box<Expr>, Vec<Expr>, Span),
    Let(Variable, Box<Expr>, Box<Expr>, Span),
    LetRec(Variable, Box<Expr>, Box<Expr>, Span),
    Assume(Variable, Type, Box<Expr>, Span),
    AssumeOver(Variable, Box<Expr>, Box<Expr>, Span),
    If(Box<Expr>, Box<Expr>, Box<Expr>, Span),
    Over(CVariable, Vec<Expr>, Span),

    // literals
    LitInt(i64, Span),
    LitBool(bool, Span),
    LitString(String, Span),
    LitDouble(f64, Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Var(_, span) => *span,
            Expr::Lam(_, _, span) => *span,
            Expr::App(_, _, span) => *span,
            Expr::Let(_, _, _, span) => *span,
            Expr::LetRec(_, _, _, span) => *span,
            Expr::Assume(_, _, _, span) => *span,
            Expr::AssumeOver(_, _, _, span) => *span,
            Expr::If(_, _, _, span) => *span,
            Expr::Over(_, _, span) => *span,
            Expr::LitInt(_, span) => *span,
            Expr::LitBool(_, span) => *span,
            Expr::LitString(_, span) => *span,
            Expr::LitDouble(_, span) => *span,
        }
    }

    pub fn with_span(mut self, new_span: Span) -> Self {
        match &mut self {
            Expr::Var(_, span) => *span = new_span,
            Expr::Lam(_, _, span) => *span = new_span,
            Expr::App(_, _, span) => *span = new_span,
            Expr::Let(_, _, _, span) => *span = new_span,
            Expr::LetRec(_, _, _, span) => *span = new_span,
            Expr::Assume(_, _, _, span) => *span = new_span,
            Expr::AssumeOver(_, _, _, span) => *span = new_span,
            Expr::If(_, _, _, span) => *span = new_span,
            Expr::Over(_, _, span) => *span = new_span,
            Expr::LitInt(_, span) => *span = new_span,
            Expr::LitBool(_, span) => *span = new_span,
            Expr::LitString(_, span) => *span = new_span,
            Expr::LitDouble(_, span) => *span = new_span,
        }
        self
    }
}

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::fmt;

use pretty::RcDoc;

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let width = 80;
        write!(f, "{}", self.pretty_print(width))
    }
}

impl Expr {
    pub fn pretty_print(&self, width: usize) -> String {
        let doc = self.to_doc();
        doc.pretty(width).to_string()
    }

    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Expr::Var(x, _) => RcDoc::text(x.clone()),

            Expr::Lam(xs, body, _) => {
                let params = if xs.len() == 1 {
                    RcDoc::text(format!("λ {} => ", xs[0])).append(body.to_doc())
                } else {
                    RcDoc::text(format!("λ ({})", xs.join(", ")))
                };
                let arrow = RcDoc::text(" => ");
                let body_doc = body.to_doc();

                // Try to put everything on one line, otherwise break
                RcDoc::group(
                    params
                        .append(arrow)
                        .append(RcDoc::line_().append(body_doc).nest(2)),
                )
            }

            Expr::App(f, args, _) => {
                if args.is_empty() {
                    return f.to_doc().append("()");
                }

                let func_doc = f.to_doc();
                let args_docs: Vec<RcDoc<()>> = args.iter().map(|arg| arg.to_doc()).collect();

                // Separate args with commas and potential line breaks
                let args_doc =
                    RcDoc::intersperse(args_docs, RcDoc::text(",").append(RcDoc::line()));

                RcDoc::group(
                    func_doc
                        .append(RcDoc::text("("))
                        .append(
                            RcDoc::line_()
                                .append(args_doc)
                                .append(RcDoc::line_())
                                .nest(2),
                        )
                        .append(RcDoc::text(")")),
                )
            }

            Expr::Let(x, binding, body, _) => RcDoc::text(format!("let {} = ", x))
                .append(binding.to_doc())
                .append(RcDoc::hardline())
                .append(body.to_doc()),

            Expr::LetRec(x, binding, body, _) => RcDoc::text(format!("let rec {} = ", x))
                .append(binding.to_doc())
                .append(RcDoc::hardline())
                .append(body.to_doc()),

            Expr::Assume(name, typ, body, _) => RcDoc::text(format!("assume {}: {}", name, typ))
                .append(RcDoc::hardline())
                .append(body.to_doc()),

            Expr::AssumeOver(name, over, body, _) => RcDoc::text(format!("assume {} = ", name))
                .append(over.to_doc())
                .append(RcDoc::hardline())
                .append(body.to_doc()),

            Expr::If(cond, thn, els, _) => RcDoc::group(
                RcDoc::text("if ")
                    .append(cond.to_doc())
                    .append(RcDoc::text(" then"))
                    .append(RcDoc::line().append(thn.to_doc()).nest(2))
                    .append(RcDoc::line())
                    .append(RcDoc::text("else"))
                    .append(RcDoc::line().append(els.to_doc()).nest(2)),
            ),

            Expr::Over(choice_var, exprs, _) => {
                let exprs_docs: Vec<RcDoc<()>> = exprs.iter().map(|e| e.to_doc()).collect();
                let exprs_doc =
                    RcDoc::intersperse(exprs_docs, RcDoc::text(",").append(RcDoc::line()));

                RcDoc::group(
                    RcDoc::text(choice_var.clone())
                        .append(RcDoc::text("‹")) // for the web target to avoid `<` being interpreted as HTML
                        .append(
                            RcDoc::line_()
                                .append(exprs_doc)
                                .append(RcDoc::line_())
                                .nest(2),
                        )
                        .append(RcDoc::text("›")),
                )
            }

            Expr::LitInt(n, _) => RcDoc::text(n.to_string()),
            Expr::LitBool(b, _) => RcDoc::text(b.to_string()),
            Expr::LitString(s, _) => RcDoc::text(format!("\"{}\"", s)),
            Expr::LitDouble(d, _) => RcDoc::text(d.to_string()),
        }
    }

    pub fn all_choices(&self) -> Result<Choices, String> {
        let mut choices = Vec::new();
        self.collect_choices(&mut choices);

        // Ensure no duplicates in the result
        let mut seen = HashMap::default();
        let mut result: Choices = Vec::new();
        for (cvar, n) in &choices {
            match seen.insert(cvar.clone(), *n) {
                None => result.push((cvar.clone(), *n)),
                Some(existing) => {
                    if existing != *n {
                        return Err(format!(
                            "Duplicate choice variable '{}' with different arities: {} and {}",
                            cvar, existing, *n
                        ));
                    }
                }
            }
        }
        Ok(result)
    }

    pub fn collect_choices(&self, choices: &mut Choices) {
        match self {
            Expr::Var(_, _)
            | Expr::LitInt(_, _)
            | Expr::LitBool(_, _)
            | Expr::LitString(_, _)
            | Expr::LitDouble(_, _) => {}
            Expr::Lam(_, body, _) => body.collect_choices(choices),
            Expr::App(f, args, _) => {
                f.collect_choices(choices);
                for arg in args {
                    arg.collect_choices(choices);
                }
            }
            Expr::Let(_, binding, body, _) | Expr::LetRec(_, binding, body, _) => {
                binding.collect_choices(choices);
                body.collect_choices(choices);
            }
            Expr::Assume(_, _, body, _) => {
                body.collect_choices(choices);
            }
            Expr::AssumeOver(_, _, body, _) => {
                // TODO: Should be resolved by the renamer now?
                body.collect_choices(choices);
            }
            Expr::If(cond, thn, els, _) => {
                cond.collect_choices(choices);
                thn.collect_choices(choices);
                els.collect_choices(choices);
            }
            Expr::Over(choice_var, exprs, _span) => {
                choices.push((choice_var.clone(), exprs.len()));
                for expr in exprs {
                    expr.collect_choices(choices);
                }
            }
        }
    }
}

// Type system with spans
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    TBase(String),
    TFun(Vec<Type>, Box<Type>),
    TVar(TVariable),
    TScheme(Vec<TVariable>, Box<Type>),
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::TBase(name) => write!(f, "{}", name),
            Type::TFun(args, res) => {
                let args_str = args
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "({}) => {}", args_str, res)
            }
            Type::TVar(x) => write!(f, "{}", x),
            Type::TScheme(vars, body) => {
                let vars_str = vars
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "∀({} ). {}", vars_str, body)
            }
        }
    }
}

impl Type {
    pub fn substitute(&self, substitution: &HashMap<TVariable, Type>) -> Type {
        match self {
            Type::TVar(var) => {
                // If the variable is in our substitution map, replace it
                substitution
                    .get(var)
                    .cloned()
                    .unwrap_or_else(|| self.clone())
            }
            Type::TBase(_) => {
                // Base types don't contain variables, return as-is
                self.clone()
            }
            Type::TFun(args, ret) => {
                // Substitute in all argument types and the return type
                let new_args = args
                    .iter()
                    .map(|arg| arg.substitute(substitution))
                    .collect();
                let new_ret = Box::new(ret.substitute(substitution));
                Type::TFun(new_args, new_ret)
            }
            Type::TScheme(vars, inner_type) => {
                // For type schemes, filter out substitutions for bound variables
                let filtered_substitution: HashMap<TVariable, Type> = substitution
                    .iter()
                    .filter(|(var, _)| !vars.contains(var))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();

                Type::TScheme(
                    vars.clone(),
                    Box::new(inner_type.substitute(&filtered_substitution)),
                )
            }
        }
    }
}

/// Parser submodule for expressions and types.
/// Use `parse_expr` defined in the super-module to parse expressions.
mod parser {
    use super::{Expr, Position, Type};
    use nom::{
        IResult,
        branch::alt,
        bytes::complete::{is_not, tag, take_until},
        character::complete::{alpha1, alphanumeric1, char, digit1, multispace1},
        combinator::{map, opt, recognize, value},
        multi::{many0, separated_list0, separated_list1},
        sequence::{delimited, pair, tuple},
    };
    use nom_locate::LocatedSpan;

    // Use nom_locate for automatic position tracking
    pub type Span<'a> = LocatedSpan<&'a str>;

    // Convert nom_locate span to our Span type
    fn to_span(start: Span, end: Span) -> super::Span {
        let start_pos = Position::new(
            start.location_offset(),
            start.location_line(),
            start.get_utf8_column() as u32,
        );
        let end_pos = Position::new(
            end.location_offset(),
            end.location_line(),
            end.get_utf8_column() as u32,
        );
        super::Span::new(start_pos, end_pos)
    }

    // Helper to create a spanned parser
    fn spanned<'a, F, O>(
        mut parser: F,
    ) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, (O, super::Span)>
    where
        F: FnMut(Span<'a>) -> IResult<Span<'a>, O>,
    {
        move |input: Span<'a>| {
            let start = input;
            let (remaining, result) = parser(input)?;
            let span = to_span(start, remaining);
            Ok((remaining, (result, span)))
        }
    }

    // Type parser - entry point that allows schemes
    pub fn type_parser(input: &str) -> IResult<&str, Type> {
        let span_input = Span::new(input);
        let (remaining, result) = type_parser_span(span_input)
            .map_err(|e| e.map(|e| nom::error::Error::new(input, e.code)))?;
        Ok((remaining.fragment(), result))
    }

    fn type_parser_span(input: Span) -> IResult<Span, Type> {
        ws(alt((forall_type, monotype_parser)))(input)
    }

    // Monotype parser - no schemes allowed (for use inside forall bodies)
    fn monotype_parser(input: Span) -> IResult<Span, Type> {
        ws(alt((function_type, atomic_type)))(input)
    }

    // Function type: (Type, Type, ...) => Type or Type => Type (no schemes in args or result)
    fn function_type(input: Span) -> IResult<Span, Type> {
        map(
            spanned(tuple((
                ws(alt((
                    // Multiple types or empty: (Type1, Type2, ...) or ()
                    delimited(
                        char('('),
                        separated_list0(ws(char(',')), ws(monotype_parser)),
                        char(')'),
                    ),
                    // Single type without parentheses: Type
                    map(atomic_type, |typ| vec![typ]),
                ))),
                ws(tag("=>")),
                ws(monotype_parser),
            ))),
            |((args, _, result), _span)| Type::TFun(args, Box::new(result)),
        )(input)
    }

    // Forall type: ∀(A, B, ...) . Type or ∀A . Type or forall(A, B, ...) . Type or forall A . Type
    fn forall_type(input: Span) -> IResult<Span, Type> {
        map(
            spanned(tuple((
                ws(alt((tag("∀"), tag("forall")))),
                ws(alt((
                    // Multiple variables or empty: (A, B, ...) or ()
                    delimited(
                        char('('),
                        separated_list1(ws(char(',')), ws(identifier)),
                        char(')'),
                    ),
                    // Single variable without parentheses: A
                    map(identifier, |var| vec![var]),
                ))),
                ws(char('.')),
                ws(monotype_parser), // Use monotype_parser to prevent nested schemes
            ))),
            |((_, vars, _, body), _span)| {
                let resolved_body = resolve_type_vars(&body, &vars.iter().collect());
                Type::TScheme(vars, Box::new(resolved_body))
            },
        )(input)
    }

    // Atomic type: base types or parenthesized monotypes
    fn atomic_type(input: Span) -> IResult<Span, Type> {
        ws(alt((base_type, parenthesized_type)))(input)
    }

    // Parenthesized type - only allows monotypes to prevent nested schemes
    fn parenthesized_type(input: Span) -> IResult<Span, Type> {
        ws(delimited(char('('), monotype_parser, char(')')))(input)
    }

    // Post-processing function to resolve type variables
    // This is a bit of a hack, since we don't actually resolve symbols.
    fn resolve_type_vars(typ: &Type, bound_vars: &std::collections::HashSet<&String>) -> Type {
        match typ {
            Type::TBase(name) => {
                if bound_vars.contains(name) {
                    Type::TVar(name.clone())
                } else {
                    Type::TBase(name.clone())
                }
            }
            Type::TFun(args, result) => {
                let resolved_args = args
                    .iter()
                    .map(|arg| resolve_type_vars(arg, bound_vars))
                    .collect();
                let resolved_result = resolve_type_vars(result, bound_vars);
                Type::TFun(resolved_args, Box::new(resolved_result))
            }
            Type::TVar(name) => Type::TVar(name.clone()),
            Type::TScheme(vars, body) => {
                // Add the new bound vars to the set
                let mut new_bound_vars = bound_vars.clone();
                for var in vars {
                    new_bound_vars.insert(var);
                }
                let resolved_body = resolve_type_vars(body, &new_bound_vars);
                Type::TScheme(vars.clone(), Box::new(resolved_body))
            }
        }
    }

    // Base type: Int, String, Bool, Double, etc.
    fn base_type(input: Span) -> IResult<Span, Type> {
        map(spanned(ws(identifier)), |(name, _span)| Type::TBase(name))(input)
    }

    // Main expression parser
    pub fn expr_parser(input: &str) -> IResult<&str, Expr> {
        let span_input = Span::new(input);
        let (remaining, result) = expr_parser_span(span_input)
            .map_err(|e| e.map(|e| nom::error::Error::new(input, e.code)))?;
        Ok((remaining.fragment(), result))
    }

    fn expr_parser_span(input: Span) -> IResult<Span, Expr> {
        ws(alt((
            if_expr,
            let_expr,
            letrec_expr,
            assume_expr,
            lambda_expr,
            app_or_atom,
        )))(input)
    }

    // Whitespace wrapper (incl. line comments)
    fn ws<'a, F, O>(inner: F) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O>
    where
        F: FnMut(Span<'a>) -> IResult<Span<'a>, O>,
    {
        delimited(ws_and_comments, inner, ws_and_comments)
    }

    // Whitespace and comments
    fn ws_and_comments(input: Span) -> IResult<Span, ()> {
        value((), many0(alt((value((), multispace1), line_comment))))(input)
    }

    // Line comment: // ... until end of line (based on nom recipes)
    fn line_comment(input: Span) -> IResult<Span, ()> {
        value((), pair(tag("//"), is_not("\n\r")))(input)
    }

    // If expression: if EXPR then EXPR else EXPR
    fn if_expr(input: Span) -> IResult<Span, Expr> {
        map(
            spanned(tuple((
                ws(tag("if")),
                expr_parser_span,
                ws(tag("then")),
                expr_parser_span,
                ws(tag("else")),
                expr_parser_span,
            ))),
            |((_, cond, _, thn, _, els), span)| {
                Expr::If(Box::new(cond), Box::new(thn), Box::new(els), span)
            },
        )(input)
    }

    // Let expression: let VAR = EXPR in EXPR or let VAR = EXPR; EXPR
    fn let_expr(input: Span) -> IResult<Span, Expr> {
        map(
            spanned(tuple((
                ws(tag("let")),
                ws(identifier),
                ws(char('=')),
                expr_parser_span,
                ws(alt((tag("in"), tag(";")))),
                expr_parser_span,
            ))),
            |((_, var, _, binding, _, body), span)| {
                Expr::Let(var, Box::new(binding), Box::new(body), span)
            },
        )(input)
    }

    // Let rec expression: let rec VAR = EXPR in EXPR or let rec VAR = EXPR; EXPR
    fn letrec_expr(input: Span) -> IResult<Span, Expr> {
        map(
            spanned(tuple((
                ws(tag("let")),
                ws(tag("rec")),
                ws(identifier),
                ws(char('=')),
                expr_parser_span,
                ws(alt((tag("in"), tag(";")))),
                expr_parser_span,
            ))),
            |((_, _, var, _, binding, _, body), span)| {
                Expr::LetRec(var, Box::new(binding), Box::new(body), span)
            },
        )(input)
    }

    use nom::combinator::cut;

    // Assume statement: assume NAME: TYPE in EXPR or assume NAME: TYPE; EXPR
    // + Assume overload statement: assume NAME = OVER in EXPR or assume NAME = OVER; EXPR
    fn assume_expr(input: Span) -> IResult<Span, Expr> {
        let (input, _) = ws(tag("assume"))(input)?;
        let (input, name) = ws(identifier)(input)?;

        alt((
            // assume NAME: TYPE - commit after seeing ':'
            map(
                spanned(tuple((
                    ws(char(':')),
                    cut(tuple((
                        ws(type_parser_span),
                        ws(alt((tag("in"), tag(";")))),
                        expr_parser_span,
                    ))),
                ))),
                |((_, (typ, _, expr)), span)| Expr::Assume(name.clone(), typ, Box::new(expr), span),
            ),
            // assume NAME = OVER - commit after seeing '='
            map(
                spanned(tuple((
                    ws(char('=')),
                    cut(tuple((
                        ws(over_expr),
                        ws(alt((tag("in"), tag(";")))),
                        expr_parser_span,
                    ))),
                ))),
                |((_, (over, _, expr)), span)| {
                    Expr::AssumeOver(name.clone(), Box::new(over), Box::new(expr), span)
                },
            ),
        ))(input)
    }

    // Lambda expression:
    //    λ (VAR, VAR, ...) => EXPR or λ VAR => EXPR
    // or \(VAR, VAR, ...) => EXPR or \VAR => EXPR
    // or fun (VAR, VAR, ...) => EXPR or fun VAR => EXPR
    fn lambda_expr(input: Span) -> IResult<Span, Expr> {
        map(
            spanned(tuple((
                ws(alt((tag("λ"), tag("\\"), tag("fun")))),
                ws(alt((
                    // Multiple parameters or empty: (param1, param2, ...) or ()
                    delimited(
                        char('('),
                        separated_list0(ws(char(',')), ws(identifier)),
                        char(')'),
                    ),
                    // Single parameter without parentheses: param
                    map(identifier, |param| vec![param]),
                ))),
                ws(tag("=>")),
                expr_parser_span,
            ))),
            |((_, params, _, body), span)| Expr::Lam(params, Box::new(body), span),
        )(input)
    }

    // Application or atomic expression - handles chained applications like f(x)(y)
    fn app_or_atom(input: Span) -> IResult<Span, Expr> {
        let (mut input, mut expr) = ws(atom_expr)(input)?;

        // Parse as many function applications as possible
        loop {
            // Try to parse function application
            let app_result = opt(ws(delimited(
                char('('),
                separated_list0(ws(char(',')), expr_parser_span),
                char(')'),
            )))(input);

            match app_result {
                Ok((remaining, Some(args))) if !args.is_empty() => {
                    let span = to_span(Span::new_extra(input.fragment(), input.extra), remaining);
                    expr = Expr::App(Box::new(expr), args, span);
                    input = remaining;
                }
                Ok((remaining, Some(_))) => {
                    // Empty args case - treat as unit application
                    let span = to_span(Span::new_extra(input.fragment(), input.extra), remaining);
                    expr = Expr::App(Box::new(expr), vec![], span);
                    input = remaining;
                }
                _ => break, // No more applications found
            }
        }

        Ok((input, expr))
    }

    // Atomic expressions
    fn atom_expr(input: Span) -> IResult<Span, Expr> {
        ws(alt((
            over_expr,
            parenthesized_expr,
            lit_bool,
            lit_number,
            lit_string,
            var_expr,
        )))(input)
    }

    // Over expression: CVAR<EXPR, EXPR, ...>
    fn over_expr(input: Span) -> IResult<Span, Expr> {
        map(
            spanned(tuple((
                ws(opt_identifier), // This will be the choice variable or an empty string
                ws(delimited(
                    alt((tag("<"), tag("‹"))), // Allow '‹' for web target to avoid HTML parsing issues
                    separated_list1(ws(char(',')), expr_parser_span),
                    alt((tag(">"), tag("›"))), // Allow '›' for web target to avoid HTML parsing issues
                )),
            ))),
            |((cvar, exprs), span)| Expr::Over(cvar, exprs, span),
        )(input)
    }

    // Parenthesized expression
    fn parenthesized_expr(input: Span) -> IResult<Span, Expr> {
        ws(delimited(char('('), expr_parser_span, char(')')))(input)
    }

    // Variable
    fn var_expr(input: Span) -> IResult<Span, Expr> {
        map(spanned(ws(identifier)), |(name, span)| {
            Expr::Var(name, span)
        })(input)
    }

    // Literals
    fn lit_bool(input: Span) -> IResult<Span, Expr> {
        map(
            spanned(ws(alt((
                value(true, tag("true")),
                value(false, tag("false")),
            )))),
            |(value, span)| Expr::LitBool(value, span),
        )(input)
    }

    fn lit_number(input: Span) -> IResult<Span, Expr> {
        map(
            spanned(ws(recognize(tuple((
                opt(char('-')),
                digit1,
                opt(tuple((char('.'), digit1))),
            ))))),
            |(s, span)| {
                if s.contains('.') {
                    match s.parse::<f64>() {
                        Ok(value) => Expr::LitDouble(value, span),
                        Err(_) => panic!("Failed to parse double: {}", s),
                    }
                } else {
                    match s.parse::<i64>() {
                        Ok(value) => Expr::LitInt(value, span),
                        Err(_) => panic!("Failed to parse int: {}", s),
                    }
                }
            },
        )(input)
    }

    fn lit_string(input: Span) -> IResult<Span, Expr> {
        map(
            spanned(ws(delimited(char('"'), take_until("\""), char('"')))),
            |(s, span)| Expr::LitString(s.to_string(), span),
        )(input)
    }

    fn is_identifier(input: Span) -> IResult<Span, Span> {
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        ))(input)
    }

    // Identifier (for variables and choice variables)
    fn identifier(input: Span) -> IResult<Span, String> {
        map(is_identifier, |s: Span| s.fragment().to_string())(input)
    }

    // Optional identifier - returns empty string if no identifier is found
    fn opt_identifier(input: Span) -> IResult<Span, String> {
        map(opt(is_identifier), |opt_id| {
            opt_id.map_or("".to_string(), |id| id.fragment().to_string())
        })(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literals() {
        assert!(matches!(parse_expr("42"), Ok(Expr::LitInt(42, _))));
        assert!(matches!(parse_expr("-42"), Ok(Expr::LitInt(-42, _))));
        assert!(matches!(parse_expr("3.41"), Ok(Expr::LitDouble(3.41, _))));
        assert!(matches!(parse_expr("-3.41"), Ok(Expr::LitDouble(-3.41, _))));
        assert!(matches!(parse_expr("0.001"), Ok(Expr::LitDouble(0.001, _))));
        assert!(matches!(
            parse_expr("-0.001"),
            Ok(Expr::LitDouble(-0.001, _))
        ));
        assert!(matches!(parse_expr("0.0"), Ok(Expr::LitDouble(0.0, _))));
        assert!(matches!(parse_expr("-0.0"), Ok(Expr::LitDouble(-0.0, _))));
        assert!(matches!(parse_expr("0"), Ok(Expr::LitInt(0, _))));
        assert!(matches!(parse_expr("-0"), Ok(Expr::LitInt(0, _))));
        assert!(parse_expr(".0").is_err());
        assert!(parse_expr("0.").is_err());

        assert!(matches!(parse_expr("true"), Ok(Expr::LitBool(true, _))));
        assert!(matches!(parse_expr("false"), Ok(Expr::LitBool(false, _))));
        assert!(matches!(parse_expr("\"hello\""), Ok(Expr::LitString(_, _))));
    }

    #[test]
    fn test_variables() {
        assert!(matches!(parse_expr("x"), Ok(Expr::Var(_, _))));
        assert!(matches!(parse_expr("foo_bar"), Ok(Expr::Var(_, _))));
    }

    #[test]
    fn test_over() {
        let result = parse_expr("choice<1, 2, 3>");
        assert!(matches!(result, Ok(Expr::Over(_, _, _))));

        let anon = parse_expr("<1, 2>");
        assert!(matches!(anon, Ok(Expr::Over(_, _, _))));

        let bad = parse_expr("1<1, 2>");
        assert!(
            bad.is_err(),
            "Should not parse choice with non-variable prefix"
        );
    }

    #[test]
    fn test_lambda() {
        let result = parse_expr("λ (x, y) => x");
        assert!(matches!(result, Ok(Expr::Lam(_, _, _))));

        let result = parse_expr("\\(x) => x");
        assert!(matches!(result, Ok(Expr::Lam(_, _, _))));

        let result = parse_expr("fun (x) => x");
        assert!(matches!(result, Ok(Expr::Lam(_, _, _))));

        let result = parse_expr("λ () => 42");
        assert!(matches!(result, Ok(Expr::Lam(_, _, _))));
    }

    #[test]
    fn test_application() {
        let result = parse_expr("f(x, y)");
        assert!(matches!(result, Ok(Expr::App(_, _, _))));
    }

    #[test]
    fn test_if() {
        let result = parse_expr("if true then 1 else 2");
        assert!(matches!(result, Ok(Expr::If(_, _, _, _))));
    }

    #[test]
    fn test_let() {
        let result = parse_expr("let x = 1 in x");
        assert!(matches!(result, Ok(Expr::Let(_, _, _, _))));

        let result = parse_expr("let rec f = λ (x) => f(x) in f");
        assert!(matches!(result, Ok(Expr::LetRec(_, _, _, _))));

        let result = parse_expr("let x = 1; x");
        assert!(matches!(result, Ok(Expr::Let(_, _, _, _))));

        let result = parse_expr("let rec f = λ (x) => f(x); f");
        assert!(matches!(result, Ok(Expr::LetRec(_, _, _, _))));
    }

    #[test]
    fn test_complex() {
        let result = parse_expr("let f = λ (x) => choice<x, 42> in f(true)");
        assert!(result.is_ok());
    }

    #[test]
    fn test_types() {
        let result = parser::type_parser("Int");
        assert!(matches!(result, Ok(("", Type::TBase(_)))));

        let result = parser::type_parser("(Int, String) => Bool");
        assert!(matches!(result, Ok(("", Type::TFun(_, _)))));

        let result = parser::type_parser("() => String");
        assert!(matches!(result, Ok(("", Type::TFun(_, _)))));

        let result = parser::type_parser("(Int) => String");
        assert!(matches!(result, Ok(("", Type::TFun(_, _)))));
    }

    #[test]
    fn test_prenex_polymorphism_enforcement() {
        // These should work - proper prenex form
        let result = parser::type_parser("∀A . A => A");
        assert!(result.is_ok());

        let result = parser::type_parser("∀(A, B) . A => B");
        assert!(result.is_ok());

        let result = parser::type_parser("forall (A, B) . (A, B) => A");
        assert!(result.is_ok());

        // These should FAIL - nested schemes not allowed
        let result = parser::type_parser("∀A . (∀B . B) => A");
        assert!(
            result.is_err(),
            "Nested forall in function argument should fail"
        );

        let result = parser::type_parser("∀A . A => (∀B . B)");
        match result {
            Ok(("", _)) => panic!("Nested forall in function result should fail"),
            Ok((_, _)) => {} // Partial parse is expected failure
            Err(_) => {}     // Parse error is expected failure
        }

        let result = parser::type_parser("(∀A . A) => Int");
        assert!(result.is_err(), "Forall in function argument should fail");

        let result = parser::type_parser("Int => (∀A . A)");
        match result {
            Ok(("", _)) => panic!("Forall in function result should fail"),
            Ok((_, _)) => {} // Partial parse is expected failure
            Err(_) => {}     // Parse error is expected failure
        }

        let result = parser::type_parser("(∀A . A, Int) => String");
        assert!(result.is_err(), "Forall in tuple argument should fail");

        // Parenthesized schemes should also fail
        let result = parser::type_parser("(∀A . A)");
        assert!(result.is_err(), "Parenthesized forall should fail");
    }

    #[test]
    fn test_assume_with_prenex_types() {
        // Valid prenex polymorphic assumptions
        let result = parse_expr("assume id: ∀A . A => A; id(42)");
        assert!(result.is_ok());

        let result = parse_expr("assume const: ∀(A, B) . A => B => A; const(1)(true)");
        assert!(result.is_ok());

        // This should fail - would require nested quantification
        // Note: This is more of a type-checking concern, but the parser should handle the syntax
        let result = parse_expr("assume bad: (∀A . A => A) => Int; bad");
        assert!(
            result.is_err(),
            "Nested forall in assume should fail at parse time"
        );
    }

    #[test]
    fn test_assume_statements() {
        let result = parse_expr("assume concat: (String, String) => String; 42");
        assert!(matches!(result, Ok(Expr::Assume(_, _, _, _))));

        let result = parse_expr("assume max: (Int, Int) => Int; 42");
        assert!(matches!(result, Ok(Expr::Assume(_, _, _, _))));

        let result = parse_expr("assume unitToString: () => String; 42");
        assert!(matches!(result, Ok(Expr::Assume(_, _, _, _))));
    }

    #[test]
    #[allow(clippy::assertions_on_constants)]
    fn test_assume_many() {
        let input = r#"
            assume concat: (String, String) => String;
            assume max: (Int, Int) => Int;
            let x = 42 in max(x, 10)
        "#;

        let result = parse_expr(input);
        match result {
            Ok(Expr::Assume(_, _, box_inner, _)) => match *box_inner {
                Expr::Assume(_, _, box_inner1, _) => match *box_inner1 {
                    Expr::Let(_, _, _, _) => {}
                    _ => assert!(false, "Expected a Let expression"),
                },
                _ => assert!(false, "Expected an Assume expression"),
            },
            _ => assert!(false, "Expected an Assume expression"),
        };
    }

    #[test]
    fn test_chained_applications() {
        // Test simple chained application
        let result = parse_expr("f(1)(2)");
        assert!(result.is_ok());
        if let Ok(Expr::App(box_expr, args, _)) = result {
            assert!(matches!(*box_expr, Expr::App(_, _, _))); // f(1)
            assert_eq!(args.len(), 1); // (2)
        }

        // Test triple chained application
        let result = parse_expr("f(1)(2)(3)");
        assert!(result.is_ok());

        // Test with the original failing case
        let result = parse_expr("let f = λ x => λ y => choice<x, y> in f(1)(2)");
        assert!(result.is_ok());

        // Test mixed applications
        let result = parse_expr("f(x, y)(z)");
        assert!(result.is_ok());

        // Test with variables
        let result = parse_expr("g(a)(b)(c)");
        assert!(result.is_ok());

        // Test single application still works
        let result = parse_expr("f(x)");
        assert!(result.is_ok());

        // Test no application still works
        let result = parse_expr("f");
        assert!(result.is_ok());
    }

    #[test]
    fn test_lambda_syntax_sugar() {
        // Test single parameter without parentheses
        let result = parse_expr("λ x => x");
        assert!(
            matches!(result, Ok(Expr::Lam(params, _, _)) if params.len() == 1 && params[0] == "x")
        );

        let result = parse_expr("\\y => y");
        assert!(
            matches!(result, Ok(Expr::Lam(params, _, _)) if params.len() == 1 && params[0] == "y")
        );

        let result = parse_expr("fun z => z");
        assert!(
            matches!(result, Ok(Expr::Lam(params, _, _)) if params.len() == 1 && params[0] == "z")
        );

        // Test that parentheses still work
        let result = parse_expr("λ (x) => x");
        assert!(
            matches!(result, Ok(Expr::Lam(params, _, _)) if params.len() == 1 && params[0] == "x")
        );

        let result = parse_expr("λ (x, y) => x");
        assert!(matches!(result, Ok(Expr::Lam(params, _, _)) if params.len() == 2));

        let result = parse_expr("λ () => 42");
        assert!(matches!(result, Ok(Expr::Lam(params, _, _)) if params.is_empty()));

        // Test nested lambdas with mixed syntax
        let result = parse_expr("λ x => λ (y, z) => x");
        assert!(result.is_ok());

        let result = parse_expr("λ (x) => λ y => x");
        assert!(result.is_ok());
    }

    #[test]
    fn test_function_type_syntax_sugar() {
        // Test single type without parentheses
        let result = parser::type_parser("Int => String");
        assert!(matches!(result, Ok(("", Type::TFun(args, _))) if args.len() == 1));

        let result = parser::type_parser("Bool => Int");
        assert!(matches!(result, Ok(("", Type::TFun(args, _))) if args.len() == 1));

        // Test that parentheses still work
        let result = parser::type_parser("(Int) => String");
        assert!(matches!(result, Ok(("", Type::TFun(args, _))) if args.len() == 1));

        let result = parser::type_parser("(Int, String) => Bool");
        assert!(matches!(result, Ok(("", Type::TFun(args, _))) if args.len() == 2));

        let result = parser::type_parser("() => String");
        assert!(matches!(result, Ok(("", Type::TFun(args, _))) if args.is_empty()));

        // Test chained function types with mixed syntax
        let result = parser::type_parser("Int => String => Bool");
        assert!(result.is_ok());
        if let Ok(("", Type::TFun(args, result_type))) = result {
            assert_eq!(args.len(), 1);
            assert!(matches!(args[0], Type::TBase(ref name) if name == "Int"));
            assert!(matches!(*result_type, Type::TFun(_, _)));
        }

        let result = parser::type_parser("(Int, String) => Bool => Double");
        assert!(result.is_ok());

        let result = parser::type_parser("Int => (String, Bool) => Double");
        assert!(result.is_ok());
    }

    #[test]
    fn test_complex_syntax_sugar_combinations() {
        // Lambda with single param and function type with single param
        let result = parse_expr("λ f => f");
        assert!(result.is_ok());

        // Assume with single type syntax
        let result = parse_expr("assume id: Int => Int; id");
        assert!(result.is_ok());

        let result =
            parse_expr("assume compose: (Int => String) => (Bool => Int) => (Bool => String); 42");
        assert!(result.is_ok());

        // Let with lambda using syntax sugar
        let result = parse_expr("let id = λ x => x in id(42)");
        assert!(result.is_ok());

        // Complex nested example
        let result = parse_expr("let f = λ x => λ y => choice<x, y> in f(1)(2)");
        assert!(result.is_ok());

        // Function type in forall with syntax sugar
        let result = parser::type_parser("∀(A, B) . A => B => A");
        assert!(result.is_ok());

        let result = parser::type_parser("forall(A) . A => A");
        assert!(result.is_ok());
    }

    #[test]
    fn test_syntax_sugar_edge_cases() {
        // Ensure that basic types still parse correctly
        let result = parser::type_parser("Int");
        assert!(matches!(result, Ok(("", Type::TBase(_)))));

        // Ensure that parenthesized types still work
        let result = parser::type_parser("(Int)");
        assert!(matches!(result, Ok(("", Type::TBase(_)))));

        // Ensure variables still work
        let result = parse_expr("x");
        assert!(matches!(result, Ok(Expr::Var(_, _))));

        // Ensure that function application still works
        let result = parse_expr("f(x)");
        assert!(matches!(result, Ok(Expr::App(_, _, _))));

        // Test precedence: function type should bind less tightly than application
        let result = parse_expr("assume f: Int => String; f(42)");
        assert!(result.is_ok());
    }

    #[test]
    fn test_forall_syntax_sugar() {
        // Test single type variable without parentheses with ∀
        let result = parser::type_parser("∀A . A");
        assert!(
            matches!(result, Ok(("", Type::TScheme(vars, _))) if vars.len() == 1 && vars[0] == "A")
        );

        // Test single type variable without parentheses with forall
        let result = parser::type_parser("forall A . A");
        assert!(
            matches!(result, Ok(("", Type::TScheme(vars, _))) if vars.len() == 1 && vars[0] == "A")
        );

        // Test that parentheses still work
        let result = parser::type_parser("∀(A) . A");
        assert!(
            matches!(result, Ok(("", Type::TScheme(vars, _))) if vars.len() == 1 && vars[0] == "A")
        );

        let result = parser::type_parser("forall(A, B) . A");
        assert!(matches!(result, Ok(("", Type::TScheme(vars, _))) if vars.len() == 2));

        // Test complex types with single variable
        let result = parser::type_parser("∀A . A => A");
        assert!(result.is_ok());
        if let Ok(("", Type::TScheme(vars, body))) = result {
            assert_eq!(vars.len(), 1);
            assert_eq!(vars[0], "A");
            assert!(matches!(*body, Type::TFun(_, _)));
        }

        // Test with function type syntax sugar combined
        let result = parser::type_parser("∀A . A => A");
        assert!(result.is_ok());

        let result = parser::type_parser("forall A . (A, A) => A");
        assert!(result.is_ok());
    }

    #[test]
    fn test_forall_edge_cases() {
        // Test that regular types still parse
        let result = parser::type_parser("Int");
        assert!(matches!(result, Ok(("", Type::TBase(_)))));

        // Test function types still work
        let result = parser::type_parser("(Int, String) => Bool");
        assert!(matches!(result, Ok(("", Type::TFun(_, _)))));

        // Test complex nested forall
        let result = parser::type_parser("∀(A, B) . (A => B) => (A => B)");
        assert!(result.is_ok());

        // Test forall with complex body
        let result = parser::type_parser("forall A . (A => Int) => (A => String) => A => Bool");
        assert!(result.is_ok());
    }

    #[test]
    fn test_all_syntax_sugar_combined() {
        // Combine forall, function type, and lambda syntax sugar
        let result = parse_expr("assume id: ∀A . A => A; let f = λ x => x in f(42)");
        assert!(result.is_ok());

        let result = parse_expr("assume const: forall (A, B) . A => B => A; const(1)(true)");
        assert!(result.is_ok());

        // Complex example with all features
        let result = parse_expr(
            r#"
            assume map: ∀(A, B) . (A => B) => A => B;
            let double = λ x => x in
            map(double)(42)
        "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_line_comments() {
        // Basic line comment
        let result = parse_expr("42 // this is a comment");
        assert!(matches!(result, Ok(Expr::LitInt(42, _))));

        // Comment at the beginning
        let result = parse_expr("// comment\n42");
        assert!(matches!(result, Ok(Expr::LitInt(42, _))));

        // Comment in expressions
        let result = parse_expr("let x = 42 // assign\nin x // use");
        assert!(matches!(result, Ok(Expr::Let(_, _, _, _))));

        // Comment without newline at end
        let result = parse_expr("42 // comment");
        assert!(matches!(result, Ok(Expr::LitInt(42, _))));

        // Comments in complex expressions
        let result = parse_expr(
            r#"
            let f = λ x => // parameter
                x // return
            in f(42) // call
        "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_assume_over() {
        let input = "assume nums = a<\"\", 0, 0.0>; nums";
        let result = parse_expr(input);
        println!("Result: {:?}", result);
        assert!(result.is_ok());
        let expr = result.unwrap();
        match expr {
            Expr::AssumeOver(name, over_expr, body, _) => {
                assert_eq!(name, "nums");
                match *over_expr {
                    Expr::Over(cvar, exprs, _) => {
                        assert_eq!(cvar, "a");
                        assert_eq!(exprs.len(), 3);
                    }
                    _ => panic!("Expected Over expression"),
                }
                match *body {
                    Expr::Var(var, _) => assert_eq!(var, "nums"),
                    _ => panic!("Expected Var(nums)"),
                }
            }
            _ => panic!("Expected AssumeOver"),
        }
    }
}
