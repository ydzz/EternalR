pub mod translate;

#[test]
fn test_first() {
   use gluon::{ThreadExt};
   let thread = gluon::new_vm();
   let number = thread.run_expr::<i32>("in","let x = 123 in x");
   println!("number {:?}",number);
}