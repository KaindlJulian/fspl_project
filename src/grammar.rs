pub type Declarations = Vec<Declaration>;
pub type Expressions = Vec<Expression>;
pub type Variables = Vec<Variable>;
pub type Parameters = Vec<(Variable, Sort)>;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Identifier {
  New(String),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Variable {
  New(String),
}

#[derive(Clone, PartialEq, Copy)]
pub enum Sort {
  Int,
  Bool,
}

#[derive(Clone)]
pub enum Program {
  New(Declarations, Identifier, Parameters, Command),
}

#[derive(Clone)]
pub enum Declaration {
  Var(Variable, Sort),
  Procedure(Identifier, Parameters, Parameters, Command),
}

#[derive(Clone)]
pub enum Command {
  None,
  VarAssignement(Variable, Expression),
  VarBlock(Variable, Sort, Box<Command>),
  Sequence(Box<Command>, Box<Command>),
  IfThenElse(Expression, Box<Command>, Box<Command>),
  IfThen(Expression, Box<Command>),
  WhileDo(Expression, Box<Command>),
  ProcedureCall(Identifier, Expressions, Variables, Vec<Sort>, Vec<Sort>),
}

#[derive(Clone)]
pub enum Expression {
  IntLiteral(i32),
  BoolLiteral(bool),
  Variable(Variable),
  Sum(Box<Expression>, Box<Expression>),
  Product(Box<Expression>, Box<Expression>),
  Difference(Box<Expression>, Box<Expression>),
  IntNegation(Box<Expression>),
  Quotient(Box<Expression>, Box<Expression>),
  LessThanEqual(Box<Expression>, Box<Expression>),
  Equality(Box<Expression>, Box<Expression>),
  BoolNegation(Box<Expression>),
  And(Box<Expression>, Box<Expression>),
  Or(Box<Expression>, Box<Expression>),
  TernaryConditional(Box<Expression>, Box<Expression>, Box<Expression>),
}

pub trait LinearRepresentation {
  fn str(&self) -> String;
}

impl LinearRepresentation for Identifier {
  fn str(&self) -> String {
    match self {
      Identifier::New(name) => name.clone(),
    }
  }
}

impl LinearRepresentation for Variable {
  fn str(&self) -> String {
    match self {
      Variable::New(name) => name.clone(),
    }
  }
}

impl LinearRepresentation for Sort {
  fn str(&self) -> String {
    match self {
      Sort::Int => String::from("int"),
      Sort::Bool => String::from("bool"),
    }
  }
}

impl LinearRepresentation for Program {
  fn str(&self) -> String {
    match self {
      Program::New(declarations, id, args, cmd) => format!(
        "{0}\nprogram {1}({2} ) {{\n{3}}}",
        declarations
          .iter()
          .map(|d| d.str())
          .collect::<Vec<String>>()
          .join("\n"),
        id.str(),
        args
          .iter()
          .map(|p| format!("{0}: {1}", p.0.str(), p.1.str()))
          .collect::<Vec<String>>()
          .join(", "),
        cmd.str(),
      ),
    }
  }
}

impl LinearRepresentation for Declaration {
  fn str(&self) -> String {
    match self {
      Declaration::Var(var, sort) => {
        format!("var {0}: {1}", var.str(), sort.str())
      }
      Declaration::Procedure(id, args, ref_args, cmd) => {
        format!(
          "procedure {0}({1}; ref {2} ) {{\n{3}\n}}\n",
          id.str(),
          args
            .iter()
            .map(|p| format!("{0}: {1}", p.0.str(), p.1.str()))
            .collect::<Vec<String>>()
            .join(", "),
          ref_args
            .iter()
            .map(|p| format!("{0}: {1}", p.0.str(), p.1.str()))
            .collect::<Vec<String>>()
            .join(", "),
          cmd.str()
        )
      }
    }
  }
}

impl LinearRepresentation for Command {
  fn str(&self) -> String {
    match self {
      Command::None => String::from(""),
      Command::VarAssignement(var, exp) => {
        format!("{0} := {1}", var.str(), exp.str())
      }
      Command::VarBlock(var, sort, cmd) => {
        format!("var {0}: {1}\n{2}", var.str(), sort.str(), cmd.str())
      }
      Command::Sequence(cmd_1, cmd_2) => {
        format!("{0}\n{1}", cmd_1.str(), cmd_2.str())
      }
      Command::IfThenElse(exp, cmd_1, cmd_2) => format!(
        "if {0} then \n{1} \nelse \n{2}",
        exp.str(),
        cmd_1.str(),
        cmd_2.str()
      ),
      Command::IfThen(exp, cmd) => {
        format!("if {0} then {{\n{1}\n}}", exp.str(), cmd.str())
      }
      Command::WhileDo(exp, cmd) => {
        format!("while {0} do {{\n{1}\n}}", exp.str(), cmd.str())
      }
      Command::ProcedureCall(id, exps, vars, _, _) => format!(
        "call {0}({1}; {2})",
        id.str(),
        exps
          .iter()
          .map(|exp| exp.str().to_owned())
          .collect::<Vec<String>>()
          .join(", "),
        vars
          .iter()
          .map(|var| var.str().to_owned())
          .collect::<Vec<String>>()
          .join(", "),
      ),
    }
  }
}

impl LinearRepresentation for Expression {
  fn str(&self) -> String {
    match self {
      Expression::IntLiteral(value) => value.to_string(),
      Expression::BoolLiteral(value) => value.to_string(),
      Expression::Variable(var) => var.str(),
      Expression::IntNegation(exp) => format!("(-{0})", exp.str()),
      Expression::Sum(exp_1, exp_2) => {
        format!("({0} + {1})", exp_1.str(), exp_2.str())
      }
      Expression::Product(exp_1, exp_2) => {
        format!("({0} * {1})", exp_1.str(), exp_2.str())
      }
      Expression::Difference(exp_1, exp_2) => {
        format!("({0} - {1})", exp_1.str(), exp_2.str())
      }
      Expression::Quotient(exp_1, exp_2) => {
        format!("({0} / {1})", exp_1.str(), exp_2.str())
      }
      Expression::LessThanEqual(exp_1, exp_2) => {
        format!("({0} <= {1})", exp_1.str(), exp_2.str())
      }
      Expression::Equality(exp_1, exp_2) => {
        format!("({0} == {1})", exp_1.str(), exp_2.str())
      }
      Expression::BoolNegation(exp) => format!("(not {0})", exp.str()),
      Expression::And(exp_1, exp_2) => {
        format!("({0} and {1})", exp_1.str(), exp_2.str())
      }
      Expression::Or(exp_1, exp_2) => {
        format!("({0} or {1})", exp_1.str(), exp_2.str())
      }
      Expression::TernaryConditional(exp_1, exp_2, exp_3) => {
        format!("({0} ? {1} : {2})", exp_1.str(), exp_2.str(), exp_3.str())
      }
    }
  }
}
