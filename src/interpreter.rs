use crate::grammar::*;
use crate::value::*;

use core::panic;
use std::collections::HashMap;
use std::hash::Hash;

pub struct Store {
  top: usize,
  memory: HashMap<usize, Value>,
}

/// The store represents the memory during program evaluation
/// the top of the stack pointer is self managed by this struct
impl Store {
  pub fn new() -> Store {
    Store {
      top: 0,
      memory: HashMap::new(),
    }
  }

  pub fn write_address(&mut self, address: usize, value: Value) {
    self.memory.insert(address, value);
  }

  pub fn read_address(&self, address: usize) -> Value {
    self.memory.get(&address).unwrap().clone()
  }

  pub fn create_and_write(&mut self, value: Value) {
    Store::write_address(self, self.top, value);
    self.top += 1;
  }

  pub fn create_and_write_sequence(&mut self, values: Vec<Value>) {
    values
      .iter()
      .for_each(|v| Store::create_and_write(self, *v));
  }

  pub fn read_sequence(&self, start: usize, end: usize) -> Vec<Value> {
    (start..=end)
      .map(|i| self.memory.get(&i).unwrap().clone())
      .collect()
  }

  pub fn create_and_write_default(&mut self, sort: &Sort) {
    match sort {
      Sort::Int => Self::create_and_write(self, INT_DEFAULT),
      Sort::Bool => Self::create_and_write(self, BOOL_DEFAULT),
    }
  }

  pub fn set_top(&mut self, new_top: usize) {
    self.top = new_top;
  }
}

/// This struct implements a environment by reference to Definition 7.14
#[derive(Clone)]
pub struct Environment {
  var: HashMap<Variable, usize>,
  proc: HashMap<ProcedureEnvironment, ProcedureSemantics>,
}

impl Environment {
  pub fn new() -> Environment {
    Environment {
      var: HashMap::new(),
      proc: HashMap::new(),
    }
  }

  pub fn from(env: Self) -> Environment {
    Environment {
      var: env.var,
      proc: env.proc,
    }
  }

  pub fn set_variable_address(&mut self, variable: &Variable, address: usize) {
    self.var.insert(variable.clone(), address);
  }

  pub fn get_variable_address(&self, variable: &Variable) -> usize {
    *self.var.get(variable).unwrap()
  }

  pub fn set_procedure_semantics(
    &mut self,
    proc_env: ProcedureEnvironment,
    proc_sem: ProcedureSemantics,
  ) {
    self.proc.insert(proc_env, proc_sem);
  }

  pub fn get_procedure_semantics(
    &self,
    proc_env: &ProcedureEnvironment,
  ) -> ProcedureSemantics {
    self.proc.get(proc_env).unwrap().clone()
  }
}

/// This struct implements a Procedure Environment to use as key
/// for Procedure Semantics in the Environment
/// Also on the basis of Definition 7.14
#[derive(Clone)]
pub struct ProcedureEnvironment {
  identifier: Identifier,
  value_param_sorts: Vec<Sort>,
  ref_param_sorts: Vec<Sort>,
}

impl ProcedureEnvironment {
  pub fn new(
    identifier: &Identifier,
    ss1: Vec<Sort>,
    ss2: Vec<Sort>,
  ) -> ProcedureEnvironment {
    ProcedureEnvironment {
      identifier: identifier.clone(),
      value_param_sorts: ss1,
      ref_param_sorts: ss2,
    }
  }
}

impl Hash for ProcedureEnvironment {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.identifier.hash(state);
  }
}

impl PartialEq for ProcedureEnvironment {
  fn eq(&self, other: &Self) -> bool {
    self.identifier == other.identifier
  }
}

impl Eq for ProcedureEnvironment {}

/// The Procedure Semantics store a copy of the environment from the
/// point of declaration aswell as the code(command) and the parameters.
/// The eval function implements the semantics.
#[derive(Clone)]
pub struct ProcedureSemantics {
  closure: Environment,
  cmd: Command,
  args: Parameters,
  ref_args: Parameters,
}

impl ProcedureSemantics {
  pub fn new(
    closure: Environment,
    cmd: Command,
    args: Parameters,
    ref_args: Parameters,
  ) -> ProcedureSemantics {
    ProcedureSemantics {
      closure,
      cmd,
      args,
      ref_args,
    }
  }

  pub fn eval(&mut self, as1: Vec<usize>, as2: Vec<usize>, store: &mut Store) {
    let mut cmd_env = Environment::from(self.closure.clone());

    as1.iter().enumerate().for_each(|(i, a)| {
      cmd_env.set_variable_address(&self.args[i].0, *a);
    });
    as2.iter().enumerate().for_each(|(i, a)| {
      cmd_env.set_variable_address(&self.ref_args[i].0, *a);
    });

    eval_command(&self.cmd, &mut cmd_env, store);
  }
}

/// Implements the semantics of a program
pub fn eval_program(
  program: &Program,
  env: &mut Environment,
  store: &mut Store,
  input: &Vec<Value>,
) -> Vec<Value> {
  match program {
    Program::New(declarations, _identifier, args, cmd) => {
      declarations
        .iter()
        .for_each(|d| eval_declaration(d, env, store));

      let main_pointer = store.top; // pointer to first local in main

      if args.len() != input.len() {
        panic!("Wrong number of inputs");
      }

      input.iter().enumerate().for_each(|(i, val)| {
        if val.is_sort(args[i].1) {
          env.set_variable_address(&args[i].0, store.top);
          store.create_and_write(*val);
        } else {
          panic!("Input has invalid type(s)");
        }
      });

      eval_command(cmd, env, store);

      store.read_sequence(main_pointer, main_pointer + args.len())
    }
  }
}

/// Implements the semantics of a declaration
fn eval_declaration(
  declaration: &Declaration,
  env: &mut Environment,
  store: &mut Store,
) {
  match declaration {
    Declaration::Var(variable, sort) => {
      env.set_variable_address(variable, store.top);
      store.create_and_write_default(sort);
    }
    Declaration::Procedure(identifier, args, ref_args, cmd) => {
      let closure = Environment::from(env.clone());
      let proc_sem = ProcedureSemantics::new(
        closure,
        cmd.clone(),
        args.clone(),
        ref_args.clone(),
      );
      let proc_env = ProcedureEnvironment::new(
        identifier,
        args.iter().map(|v| v.1).collect(),
        ref_args.iter().map(|v| v.1).collect(),
      );
      env.set_procedure_semantics(proc_env, proc_sem);
    }
  }
}

/// Implements the semantics of a command
fn eval_command(cmd: &Command, env: &mut Environment, store: &mut Store) {
  match cmd {
    Command::None => (),
    Command::VarAssignement(variable, exp) => {
      let value = eval_exp(exp, env, store);
      let address = env.get_variable_address(variable);
      store.write_address(address, value);
    }
    Command::VarBlock(variable, sort, c) => {
      let mut cmd_env = Environment::from(env.clone());
      cmd_env.set_variable_address(variable, store.top);
      store.create_and_write_default(sort);
      eval_command(c, &mut cmd_env, store);
    }
    Command::Sequence(c1, c2) => {
      eval_command(c1, env, store);
      eval_command(c2, env, store);
    }
    Command::IfThenElse(exp, c1, c2) => {
      if let Value::Bool(is_true) = eval_exp(exp, env, store) {
        if is_true {
          eval_command(c1, env, store);
        } else {
          eval_command(c2, env, store);
        }
      }
    }
    Command::IfThen(exp, c) => {
      if let Value::Bool(is_true) = eval_exp(exp, env, store) {
        if is_true {
          eval_command(c, env, store);
        }
      }
    }
    Command::WhileDo(exp, c) => {
      while unwrap_bool(eval_exp(exp, env, store)) {
        eval_command(c, env, store);
      }
    }
    Command::ProcedureCall(identifier, exps, vars, ss1, ss2) => {
      let old_top = store.top;

      let val_addresses: Vec<usize> =
        (store.top..(store.top + exps.len())).collect();

      let ref_addresses: Vec<usize> = vars
        .iter()
        .map(|variable| env.get_variable_address(variable))
        .collect();

      let values: Vec<Value> =
        exps.iter().map(|exp| eval_exp(exp, env, store)).collect();
      store.create_and_write_sequence(values); //increments stack pointer

      let proc_env =
        ProcedureEnvironment::new(identifier, ss1.clone(), ss2.clone());

      let mut proc_sem = env.get_procedure_semantics(&proc_env);
      proc_sem.eval(val_addresses, ref_addresses, store);

      store.set_top(old_top);
    }
  }
}

/// Implements the semantics of an Expression
fn eval_exp(
  exp: &Expression,
  env: &mut Environment,
  store: &mut Store,
) -> Value {
  match exp {
    Expression::IntLiteral(value) => Value::Int(*value),
    Expression::BoolLiteral(value) => Value::Bool(*value),
    Expression::Variable(var) => {
      store.read_address(env.get_variable_address(var))
    }
    Expression::Sum(e1, e2) => {
      eval_exp(e1, env, store) + eval_exp(e2, env, store)
    }
    Expression::Product(e1, e2) => {
      eval_exp(e1, env, store) * eval_exp(e2, env, store)
    }
    Expression::Difference(e1, e2) => {
      eval_exp(e1, env, store) - eval_exp(e2, env, store)
    }
    Expression::IntNegation(e) => -eval_exp(e, env, store),
    Expression::Quotient(e1, e2) => {
      eval_exp(e1, env, store) / eval_exp(e2, env, store)
    }
    Expression::LessThanEqual(e1, e2) => {
      Value::Bool(eval_exp(e1, env, store) <= eval_exp(e2, env, store))
    }
    Expression::Equality(e1, e2) => {
      Value::Bool(eval_exp(e1, env, store) == eval_exp(e2, env, store))
    }
    Expression::BoolNegation(e) => !eval_exp(e, env, store),
    Expression::And(e1, e2) => {
      logical_and(eval_exp(e1, env, store), eval_exp(e2, env, store))
    }
    Expression::Or(e1, e2) => {
      logical_or(eval_exp(e1, env, store), eval_exp(e2, env, store))
    }
    Expression::TernaryConditional(e1, e2, e3) => {
      if let Value::Bool(is_true) = eval_exp(e1, env, store) {
        if is_true {
          return eval_exp(e2, env, store);
        } else {
          return eval_exp(e3, env, store);
        }
      }
      panic!()
    }
  }
}
