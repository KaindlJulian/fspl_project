use fspl::examples::*;
use fspl::grammar::*;
use fspl::interpreter::*;
use fspl::types::*;
use fspl::value::*;

fn main() {
  let mut program = factorial();

  println!("{}", program.str());

  let mut context: TypeContext = TypeContext::new();
  let result: TypeResult<Tag> = program.check(&mut context);

  match result {
    Ok(tag) => println!("\nTYPE CHECK: VALID {}", tag),
    Err(()) => println!("\nTYPE CHECK: ERROR"),
  }

  let mut env = Environment::new();
  let mut store = Store::new();

  let input = vec![Value::Int(5), Value::Int(0)];
  let output = eval_program(&program, &mut env, &mut store, &input);

  println!("\nINPUT\tOUTPUT");
  for i in 0..input.len() {
    println!("{0}\t{1}", input[i], output[i]);
  }
}
