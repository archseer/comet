//! Abstract syntax tree representation

#[derive(Eq, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Op(Box<Expr>, Opcode, Box<Expr>),
    // AtomExpr
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Expr::*;
        match self {
            Literal(n) => write!(fmt, "{:?}", n),
            Op(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            // Error => write!(fmt, "error"),
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum Literal {
    Int(i64),
}

impl std::fmt::Debug for Literal {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        use self::Literal::*;
        match *self {
            Int(n) => write!(fmt, "{:?}", n),
        }
    }
}

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

// pub enum AtomExpr {
//     ArrayExpr {
//         ...
//     },
//     TupleExpr {
//         ...
//     },

// }
