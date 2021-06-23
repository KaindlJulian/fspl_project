use crate::grammar::*;

// see examples_output.txt for outputs

/// a valid program
pub fn factorial() -> Program {
  Program::New(
    vec![Declaration::Procedure(
      Identifier::New(String::from("is_greater_one")),
      vec![(Variable::New(String::from("n")), Sort::Int)],
      vec![(Variable::New(String::from("r")), Sort::Bool)],
      Command::VarAssignement(
        Variable::New(String::from("r")),
        Expression::BoolNegation(Box::new(Expression::LessThanEqual(
          Box::new(Expression::Variable(Variable::New(String::from("n")))),
          Box::new(Expression::IntLiteral(1)),
        ))),
      ),
    )],
    Identifier::New(String::from("factorial")),
    vec![
      (Variable::New(String::from("value")), Sort::Int),
      (Variable::New(String::from("result")), Sort::Int),
    ],
    Command::Sequence(
      Box::new(Command::VarAssignement(
        Variable::New(String::from("result")),
        Expression::IntLiteral(1),
      )),
      Box::new(Command::Sequence(
        Box::new(Command::VarBlock(
          Variable::New(String::from("greater_one")),
          Sort::Bool,
          Box::new(Command::Sequence(
            Box::new(Command::ProcedureCall(
              Identifier::New(String::from("is_greater_one")),
              vec![Expression::Variable(Variable::New(String::from("value")))],
              vec![Variable::New(String::from("greater_one"))],
              vec![],
              vec![],
            )),
            Box::new(Command::WhileDo(
              Expression::Variable(Variable::New(String::from("greater_one"))),
              Box::new(Command::Sequence(
                Box::new(Command::VarAssignement(
                  Variable::New(String::from("result")),
                  Expression::Product(
                    Box::new(Expression::Variable(Variable::New(
                      String::from("result"),
                    ))),
                    Box::new(Expression::Variable(Variable::New(
                      String::from("value"),
                    ))),
                  ),
                )),
                Box::new(Command::Sequence(
                  Box::new(Command::VarAssignement(
                    Variable::New(String::from("value")),
                    Expression::Difference(
                      Box::new(Expression::Variable(Variable::New(
                        String::from("value"),
                      ))),
                      Box::new(Expression::IntLiteral(1)),
                    ),
                  )),
                  Box::new(Command::ProcedureCall(
                    Identifier::New(String::from("is_greater_one")),
                    vec![Expression::Variable(Variable::New(String::from(
                      "value",
                    )))],
                    vec![Variable::New(String::from("greater_one"))],
                    vec![],
                    vec![],
                  )),
                )),
              )),
            )),
          )),
        )),
        Box::new(Command::None),
      )),
    ),
  )
}

/// not well formed because we try to assign to variable "r" in the program
/// but it is not in the list variable typings
pub fn error_factorial() -> Program {
  Program::New(
    vec![Declaration::Procedure(
      Identifier::New(String::from("is_greater_one")),
      vec![(Variable::New(String::from("n")), Sort::Int)],
      vec![(Variable::New(String::from("r")), Sort::Bool)],
      Command::VarAssignement(
        Variable::New(String::from("r")),
        Expression::BoolNegation(Box::new(Expression::LessThanEqual(
          Box::new(Expression::Variable(Variable::New(String::from("n")))),
          Box::new(Expression::IntLiteral(1)),
        ))),
      ),
    )],
    Identifier::New(String::from("factorial")),
    vec![(Variable::New(String::from("value")), Sort::Int)],
    Command::VarBlock(
      Variable::New(String::from("result")),
      Sort::Int,
      Box::new(Command::Sequence(
        Box::new(Command::VarAssignement(
          Variable::New(String::from("r")),
          Expression::IntLiteral(1),
        )),
        Box::new(Command::Sequence(
          Box::new(Command::VarBlock(
            Variable::New(String::from("greater_one")),
            Sort::Bool,
            Box::new(Command::Sequence(
              Box::new(Command::ProcedureCall(
                Identifier::New(String::from("is_greater_one")),
                vec![Expression::Variable(Variable::New(String::from(
                  "value",
                )))],
                vec![Variable::New(String::from("greater_one"))],
                vec![],
                vec![],
              )),
              Box::new(Command::WhileDo(
                Expression::Variable(Variable::New(String::from(
                  "greater_one",
                ))),
                Box::new(Command::Sequence(
                  Box::new(Command::VarAssignement(
                    Variable::New(String::from("result")),
                    Expression::Product(
                      Box::new(Expression::Variable(Variable::New(
                        String::from("result"),
                      ))),
                      Box::new(Expression::Variable(Variable::New(
                        String::from("value"),
                      ))),
                    ),
                  )),
                  Box::new(Command::Sequence(
                    Box::new(Command::VarAssignement(
                      Variable::New(String::from("value")),
                      Expression::Difference(
                        Box::new(Expression::Variable(Variable::New(
                          String::from("value"),
                        ))),
                        Box::new(Expression::IntLiteral(1)),
                      ),
                    )),
                    Box::new(Command::ProcedureCall(
                      Identifier::New(String::from("is_greater_one")),
                      vec![Expression::Variable(Variable::New(String::from(
                        "value",
                      )))],
                      vec![Variable::New(String::from("greater_one"))],
                      vec![],
                      vec![],
                    )),
                  )),
                )),
              )),
            )),
          )),
          Box::new(Command::None),
        )),
      )),
    ),
  )
}

/// a valid expression
pub fn simple_expression() -> Expression {
  Expression::Sum(
    Box::new(Expression::TernaryConditional(
      Box::new(Expression::LessThanEqual(
        Box::new(Expression::IntLiteral(1)),
        Box::new(Expression::IntLiteral(5)),
      )),
      Box::new(Expression::IntLiteral(5)),
      Box::new(Expression::IntLiteral(15)),
    )),
    Box::new(Expression::IntLiteral(5)),
  )
}

/// not well formed expression
/// because the first sub expression of the
/// ternary operator is not of sort bool
pub fn error_simple_expression() -> Expression {
  Expression::Sum(
    Box::new(Expression::TernaryConditional(
      Box::new(Expression::Sum(
        Box::new(Expression::IntLiteral(1)),
        Box::new(Expression::IntLiteral(5)),
      )),
      Box::new(Expression::IntLiteral(5)),
      Box::new(Expression::IntLiteral(15)),
    )),
    Box::new(Expression::IntLiteral(5)),
  )
}
