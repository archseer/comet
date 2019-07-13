//! Abstract syntax tree representation
use crate::diagnostics::Span;
use crate::symbol::Symbol;

/// An identifier.
#[derive(Clone, Eq, PartialEq)]
pub struct Ident(pub Symbol);

impl std::fmt::Debug for Ident {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", self.0)
    }
}

pub struct Module {
    // unique_id
    // pub name: String,
    pub statements: Vec<Statement>,
}

pub enum Statement {
    /// Function declaration.
    Fn {
        name: Ident,
        args: Vec<Ident>, // TODO: type these in the future
        body: Vec<Expr>,
    },
    // /// Expression statement.
    // Expression(Expr),
}

impl std::fmt::Debug for Statement {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Statement::*;
        match &self {
            Fn { name, body, args } => write!(fmt, "fn {}({:?}) -> {{ {:?} }}", name.0, args, body),
            // Expression(expr) => write!(fmt, "{:?}", expr),
            // Error => write!(fmt, "error"),
        }
    }
}

/// A valid expression.
#[derive(Clone, Eq, PartialEq)]
pub enum Expr {
    /// An integer literal.
    Int(i64),
    /// A boolean
    Bool(bool),
    // String()
    /// () unit type.
    Unit,
    /// Binary operator application.
    Op(Box<Expr>, Opcode, Box<Expr>),
    /// Variable reference.
    Var(Ident),
    /// Function call.
    Call {
        name: Ident,
        args: Vec<Expr>,
    },
    /// Variable declaration.
    Let {
        name: Ident,
        ty: Option<Ident>,
        value: Box<Expr>,
    },
    /// If-then-else conditional.
    If(Box<Expr>, Vec<Expr>, Vec<Expr>),
    /// Lambda definition
    Lambda {
        args: Vec<Ident>,
        body: Vec<Expr>,
    },
    // /// An unknown expression.
    // Unknown
    Error,
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Expr::*;
        match &self {
            Int(n) => write!(fmt, "{:?}", n),
            Bool(b) => write!(fmt, "{:?}", b),
            Unit => write!(fmt, "()"),
            Op(l, op, r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            Var(i) => write!(fmt, "{:?}", i),
            Call { ref name, ref args } => write!(fmt, "{}({:?})", name.0, args),
            Let { name, value, .. } => write!(fmt, "let {} = {:?}", name.0, value),
            If(cond, t, f) => write!(fmt, "if {:?} {{ {:?} }} else {{ {:?} }}", cond, t, f),
            Lambda { body, args } => write!(fmt, "fn ({:?}) -> {{ {:?} }}", args, body),
            Error => write!(fmt, "error"),
        }
    }
}

/// A binary operator.
#[derive(Clone, Eq, PartialEq)]
pub enum Opcode {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Band,
    Bor,
    Less,
    Greater,
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Pipe,
    And,
    Or,
    Xor,
    Not,
}

impl Opcode {
    pub fn to_string(&self) -> &str {
        use Opcode::*;

        match self {
            Mul => "*",
            Div => "/",
            Mod => "%",
            Add => "+",
            Sub => "-",
            Band => "&",
            Bor => "|",
            Less => "<",
            Greater => ">",
            Equal => "==",
            NotEqual => "!=",
            LessEqual => "<=",
            GreaterEqual => ">=",
            Pipe => "|>",
            And => "&&",
            Or => "||",
            Xor => "^",
            Not => "!",
        }
    }
}

// Operators are evaluated based on the operator precedence outlined
// [here](https://doc.rust-lang.org/reference.html#operator-precedence).

// The full list of supported operators and functions is as follows:

// `*`, `/`, `%`, `+`, `-`, `<<`, `>>`, `&`, `^`, `|`, `==`, `!=`, `<=`, `>=`, `<`, `>`, `cmp`, `sqr`, `abs`, `cube`, `pow`, `min`, `max`

// TODO: should not_eq, less or eq etc be defined in terms of above?

impl std::fmt::Debug for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match (*self).clone() {
            Opcode::Add => write!(f, "+"),
            Opcode::Sub => write!(f, "-"),
            Opcode::Mul => write!(f, "*"),
            Opcode::Div => write!(f, "/"),
            Opcode::Mod => write!(f, "%"),
            Opcode::Less => write!(f, "<"),
            Opcode::Greater => write!(f, ">"),
            Opcode::Equal => write!(f, "=="),
            Opcode::NotEqual => write!(f, "!="),
            Opcode::LessEqual => write!(f, "<="),
            Opcode::GreaterEqual => write!(f, ">="),
            Opcode::Not => write!(f, "!"),
            Opcode::Band => write!(f, "&"),
            Opcode::Bor => write!(f, "|"),
            Opcode::And => write!(f, "&&"),
            Opcode::Or => write!(f, "||"),
            Opcode::Xor => write!(f, "^"),
            Opcode::Pipe => write!(f, "|>"),
        }
    }
}
