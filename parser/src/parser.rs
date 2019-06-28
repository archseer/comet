#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn expr_test() {
        use crate::ast::*;
        use crate::grammar::ExprParser;

        assert_eq!(Ok(Expr::Int(42)), ExprParser::new().parse("42"));

        assert_eq!(Ok(Expr::Int(255)), ExprParser::new().parse("0xFF"));

        assert_eq!(Ok(Expr::Int(129)), ExprParser::new().parse("0b1000_0001"));

        assert_eq!(Ok(Expr::Bool(true)), ExprParser::new().parse("true"));

        assert_eq!(Ok(Expr::Bool(false)), ExprParser::new().parse("false"));

        let expr = ExprParser::new().parse("3 + 2 * 4").unwrap();
        assert_eq!(&format!("{:?}", expr), "(3 + (2 * 4))");

        // assert_eq!(
        //     Ok(Expr::Int(129)),
        //     ExprParser::new().parse("fn (a, b, c) -> 1 + 2 * 3")
        // );

        // assert_eq!(
        //     Ok(Expr::Int(129)),
        //     ExprParser::new().parse("test(1 + 2, 2, 3)")
        // );

        assert_eq!(
            Ok(Expr::Int(12)),
            ExprParser::new().parse(
                "fn (a, b) {
                    let x = 1 + 2
                    x
 
                    if x > 2 {
                        1
                    } else {
                        0
                    }
                }"
            )
        );
    }
}
