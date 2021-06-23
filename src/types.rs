use crate::grammar::*;
use std::collections::HashMap;

pub type TypeResult<Tag> = std::result::Result<Tag, ()>;

pub enum Tag {
  Program,
  Declaration,
  Command,
  Expression(Sort),
  Variable,
  Identifier,
}

impl std::fmt::Display for Tag {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match *self {
      Tag::Program => {
        write!(f, "Program")
      }
      Tag::Declaration => {
        write!(f, "Declaration")
      }
      Tag::Command => {
        write!(f, "Command")
      }
      Tag::Expression(_) => {
        write!(f, "Expression")
      }
      Tag::Variable => {
        write!(f, "Variable")
      }
      Tag::Identifier => {
        write!(f, "Identifier")
      }
    }
  }
}

#[derive(Clone)]
pub struct TypeContext {
  variable_typing: HashMap<Variable, Sort>,
  procedure_typing: HashMap<Identifier, Vec<Sort>>,
}

impl TypeContext {
  pub fn new() -> TypeContext {
    TypeContext {
      variable_typing: HashMap::new(),
      procedure_typing: HashMap::new(),
    }
  }
}

pub trait TypeCheck {
  fn check(&mut self, context: &mut TypeContext) -> TypeResult<Tag>;
}

impl TypeCheck for Identifier {
  fn check(&mut self, _context: &mut TypeContext) -> TypeResult<Tag> {
    check_empty_string(self, Tag::Identifier)
  }
}

impl TypeCheck for Variable {
  fn check(&mut self, _context: &mut TypeContext) -> TypeResult<Tag> {
    check_empty_string(self, Tag::Variable)
  }
}

impl TypeCheck for Program {
  fn check(&mut self, context: &mut TypeContext) -> TypeResult<Tag> {
    match self {
      Program::New(declarations, id, args, cmd) => {
        check_vec(declarations, context)?;
        id.check(context)?;
        let mut program_context = TypeContext::new();

        args
          .iter_mut()
          .map(|p| {
            p.0.check(&mut program_context)?;
            add_variable_typing(&p.0, &p.1, &mut program_context)?;
            Ok(())
          })
          .collect::<Result<(), ()>>()?;

        cmd.check(&mut program_context)?;
      }
    }
    Ok(Tag::Program)
  }
}

impl TypeCheck for Declaration {
  fn check(&mut self, context: &mut TypeContext) -> TypeResult<Tag> {
    let mut procedure_context = TypeContext::new();
    match self {
      Declaration::Var(var, sort) => {
        var.check(context)?;
        add_variable_typing(var, sort, &mut procedure_context)?;
      }
      Declaration::Procedure(id, args, refs, cmd) => {
        id.check(context)?;

        let mut s_1 = args
          .iter_mut()
          .map(|p| {
            p.0.check(&mut procedure_context)?;
            add_variable_typing(&p.0, &p.1, &mut procedure_context)?;
            Ok(p.1)
          })
          .collect::<Result<Vec<Sort>, ()>>()?;

        let mut s_2 = refs
          .iter_mut()
          .map(|p| {
            p.0.check(&mut procedure_context)?;
            add_variable_typing(&p.0, &p.1, &mut procedure_context)?;
            Ok(p.1)
          })
          .collect::<Result<Vec<Sort>, ()>>()?;

        s_1.append(&mut s_2);

        add_procedure_typing(id, s_1, context)?;
        cmd.check(&mut procedure_context)?;
      }
    }
    Ok(Tag::Declaration)
  }
}

impl TypeCheck for Command {
  fn check(&mut self, context: &mut TypeContext) -> TypeResult<Tag> {
    match self {
      Command::None => (),
      Command::VarAssignement(var, exp) => {
        let e_sort = check_exp(exp, context)?;
        check_variable_sort(var, &e_sort, context)?;
      }
      Command::VarBlock(var, sort, cmd) => {
        var.check(context)?;
        add_variable_typing(var, sort, context)?;
        cmd.check(context)?;
      }
      Command::Sequence(cmd_1, cmd_2) => {
        cmd_1.check(context)?;
        cmd_2.check(context)?;
      }
      Command::IfThenElse(exp, cmd_1, cmd_2) => {
        if check_exp(exp, context)? != Sort::Bool {
          return Err(());
        }
        cmd_1.check(context)?;
        cmd_2.check(context)?;
      }
      Command::IfThen(exp, cmd) | Command::WhileDo(exp, cmd) => {
        if check_exp(exp, context)? != Sort::Bool {
          return Err(());
        }
        cmd.check(context)?;
      }
      Command::ProcedureCall(id, exps, vars, ss1, ss2) => {
        id.check(context)?;
        let mut s_1: Vec<Sort> = exps
          .iter_mut()
          .map(|e| check_exp(e, context))
          .collect::<Result<Vec<Sort>, ()>>()
          .unwrap_or(vec![]);
        ss1.append(&mut s_1.clone());

        let mut s_2: Vec<Sort> = vars
          .iter_mut()
          .map(|var| check_variable(var, context))
          .collect::<Result<Vec<Sort>, ()>>()
          .unwrap_or(vec![]);
        ss2.append(&mut s_2.clone());

        s_1.append(&mut s_2);

        check_procedure_sorts(id, &s_1, context)?;
      }
    }
    Ok(Tag::Command)
  }
}

impl TypeCheck for Expression {
  fn check(&mut self, context: &mut TypeContext) -> TypeResult<Tag> {
    match self {
      Expression::IntLiteral(_val) => Ok(Tag::Expression(Sort::Int)),
      Expression::BoolLiteral(_val) => Ok(Tag::Expression(Sort::Bool)),
      Expression::Variable(var) => {
        let sort = check_variable(var, context)?;
        Ok(Tag::Expression(sort))
      }
      Expression::Sum(e_1, e_2)
      | Expression::Difference(e_1, e_2)
      | Expression::Product(e_1, e_2)
      | Expression::Quotient(e_1, e_2) => {
        let s: Sort = check_exp_same_sort(e_1, e_2, context)?;
        if s == Sort::Int {
          Ok(Tag::Expression(s))
        } else {
          Err(())
        }
      }
      Expression::LessThanEqual(e_1, e_2) => {
        let s: Sort = check_exp_same_sort(e_1, e_2, context)?;
        if s == Sort::Int {
          Ok(Tag::Expression(Sort::Bool))
        } else {
          Err(())
        }
      }
      Expression::IntNegation(e) => {
        let s = check_exp(e, context)?;
        if s == Sort::Int {
          Ok(Tag::Expression(s))
        } else {
          Err(())
        }
      }
      Expression::Equality(e_1, e_2)
      | Expression::And(e_1, e_2)
      | Expression::Or(e_1, e_2) => {
        let s: Sort = check_exp_same_sort(e_1, e_2, context)?;
        if s == Sort::Bool {
          Ok(Tag::Expression(s))
        } else {
          Err(())
        }
      }
      Expression::BoolNegation(e) => {
        let s = check_exp(e, context)?;
        if s == Sort::Bool {
          Ok(Tag::Expression(s))
        } else {
          Err(())
        }
      }
      Expression::TernaryConditional(e_1, e_2, e_3) => {
        let result_sort = check_exp_same_sort(e_2, e_3, context)?;
        let sort = check_exp(e_1, context)?;
        if sort == Sort::Bool {
          Ok(Tag::Expression(result_sort))
        } else {
          Err(())
        }
      }
    }
  }
}

/// typecheck for list of checkable values
fn check_vec<T: TypeCheck>(
  list: &mut Vec<T>,
  context: &mut TypeContext,
) -> Result<(), ()> {
  let res: Result<Vec<Tag>, _> =
    list.iter_mut().map(|d| d.check(context)).collect();
  match res {
    Err(_) => Err(()),
    Ok(_vec) => Ok(()),
  }
}

/// typecheck for linear representation
/// errors if the linear representation is an empty string
fn check_empty_string<T: LinearRepresentation>(
  c: &T,
  tag: Tag,
) -> TypeResult<Tag> {
  if c.str().trim().is_empty() {
    Err(())
  } else {
    Ok(tag)
  }
}

/// add variable typing
/// errors if key already exists
fn add_variable_typing(
  var: &Variable,
  sort: &Sort,
  context: &mut TypeContext,
) -> Result<(), ()> {
  if context.variable_typing.get(var).is_some() {
    return Err(());
  }
  context.variable_typing.insert(var.clone(), sort.clone());
  Ok(())
}

/// add procedure typing
/// errors if key already exists
fn add_procedure_typing(
  id: &Identifier,
  sorts: Vec<Sort>,
  context: &mut TypeContext,
) -> Result<(), ()> {
  if let Some(_sorts) = context.procedure_typing.get_mut(id) {
    return Err(());
  } else {
    context.procedure_typing.insert(id.clone(), sorts);
  }
  Ok(())
}

/// check if variable exists in context and return sort
fn check_variable(var: &Variable, context: &TypeContext) -> Result<Sort, ()> {
  if let Some(sort) = context.variable_typing.get(var) {
    Ok(sort.clone())
  } else {
    Err(())
  }
}

/// check if variable typing has given sort in context
fn check_variable_sort(
  var: &Variable,
  sort: &Sort,
  context: &TypeContext,
) -> Result<(), ()> {
  if let Some(s) = context.variable_typing.get(var) {
    if sort == s {
      Ok(())
    } else {
      Err(())
    }
  } else {
    return Err(());
  }
}

/// check if procedure typing has given sorts in context
fn check_procedure_sorts(
  id: &Identifier,
  sorts: &Vec<Sort>,
  context: &TypeContext,
) -> Result<(), ()> {
  if let Some(stored) = context.procedure_typing.get(id) {
    if stored.iter().zip(sorts.iter()).any(|(s_1, s_2)| s_1 != s_2) {
      return Err(());
    }
  }
  Ok(())
}

/// check if two given expressions have the same sort
fn check_exp_same_sort(
  e_1: &mut Expression,
  e_2: &mut Expression,
  context: &mut TypeContext,
) -> Result<Sort, ()> {
  let s_1: Sort;
  let s_2: Sort;
  match e_1.check(context)? {
    Tag::Expression(s) => s_1 = s,
    _ => return Err(()),
  }
  match e_2.check(context)? {
    Tag::Expression(s) => s_2 = s,
    _ => return Err(()),
  }
  if (s_1 == Sort::Int) && (s_2 == Sort::Int) {
    Ok(s_1)
  } else {
    Err(())
  }
}

/// checks an exrpession and returns the sort
/// errors if expression typecheck failed
fn check_exp(
  e: &mut Expression,
  context: &mut TypeContext,
) -> Result<Sort, ()> {
  let sort: Sort;

  match e.check(context)? {
    Tag::Expression(s) => sort = s,
    _ => return Err(()),
  }

  Ok(sort)
}
