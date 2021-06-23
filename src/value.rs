use crate::grammar::Sort;

use core::panic;

pub const INT_DEFAULT: Value = Value::Int(0);
pub const BOOL_DEFAULT: Value = Value::Bool(false);

/// The value enum used in the interpreter
#[derive(Clone, Copy)]
pub enum Value {
  Int(i32),
  Bool(bool),
}

impl Value {
  pub fn is_sort(&self, sort: Sort) -> bool {
    match self {
      Value::Int(_) => sort == Sort::Int,
      Value::Bool(_) => sort == Sort::Bool,
    }
  }
}

// Here we define operations and auxiliary functions on the enum to keep the interpreter code clean

fn int_operation(lhs: Value, rhs: Value, f: fn(i32, i32) -> i32) -> Value {
  if let Value::Int(a) = lhs {
    if let Value::Int(b) = rhs {
      return Value::Int(f(a, b));
    }
  }
  panic!();
}

impl std::ops::Add for Value {
  type Output = Value;
  fn add(self, rhs: Self) -> Self::Output {
    int_operation(self, rhs, |a, b| a + b)
  }
}

impl std::ops::Neg for Value {
  type Output = Value;
  fn neg(self) -> Self::Output {
    if let Value::Int(a) = self {
      return Value::Int(-a);
    }
    panic!();
  }
}

impl std::ops::Mul for Value {
  type Output = Value;
  fn mul(self, rhs: Self) -> Self::Output {
    int_operation(self, rhs, |a, b| a * b)
  }
}

impl std::ops::Sub for Value {
  type Output = Value;
  fn sub(self, rhs: Self) -> Self::Output {
    int_operation(self, rhs, |a, b| a - b)
  }
}

impl std::ops::Div for Value {
  type Output = Value;
  fn div(self, rhs: Self) -> Self::Output {
    int_operation(self, rhs, |a, b| a / b)
  }
}

impl PartialEq for Value {
  fn eq(&self, other: &Self) -> bool {
    if let Value::Int(a) = self {
      if let Value::Int(b) = other {
        return a == b;
      }
    }
    if let Value::Bool(a) = self {
      if let Value::Bool(b) = other {
        return a == b;
      }
    }
    panic!();
  }
}

impl PartialOrd for Value {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    if let Value::Int(a) = self {
      if let Value::Int(b) = other {
        return a.partial_cmp(b);
      }
    }
    if let Value::Bool(a) = self {
      if let Value::Bool(b) = other {
        return a.partial_cmp(b);
      }
    }
    panic!();
  }
}

impl std::ops::Not for Value {
  type Output = Value;
  fn not(self) -> Self::Output {
    if let Value::Bool(a) = self {
      return Value::Bool(!a);
    }
    panic!();
  }
}

pub fn logical_and(lhs: Value, rhs: Value) -> Value {
  if let Value::Bool(a) = lhs {
    if let Value::Bool(b) = rhs {
      return Value::Bool(a && b);
    }
  }
  panic!();
}

pub fn logical_or(lhs: Value, rhs: Value) -> Value {
  if let Value::Bool(a) = lhs {
    if let Value::Bool(b) = rhs {
      return Value::Bool(a || b);
    }
  }
  panic!();
}

pub fn unwrap_bool(value: Value) -> bool {
  match value {
    Value::Bool(v) => return v,
    Value::Int(_) => panic!(),
  }
}

pub fn unwrap_int(value: Value) -> i32 {
  match value {
    Value::Int(v) => return v,
    Value::Bool(_) => panic!(),
  }
}

impl std::fmt::Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match *self {
      Value::Int(val) => write!(f, "{0}", val),
      Value::Bool(val) => write!(f, "{0}", val),
    }
  }
}
