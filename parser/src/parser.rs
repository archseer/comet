#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn expr_test() {
        use crate::ast::*;
        use crate::grammar::ExprParser;

        assert_eq!(
            Ok(Expr::Literal(Literal::Int(42))),
            ExprParser::new().parse("42")
        );

        assert_eq!(
            Ok(Expr::Literal(Literal::Int(255))),
            ExprParser::new().parse("0xFF")
        );

        assert_eq!(
            Ok(Expr::Literal(Literal::Int(129))),
            ExprParser::new().parse("0b1000_0001")
        );

        let expr = ExprParser::new().parse("3 + 2 * 4").unwrap();
        assert_eq!(&format!("{:?}", expr), "(3 + (2 * 4))");
    }
}
