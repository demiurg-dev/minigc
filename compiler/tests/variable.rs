//! Let-expressions and variable usage.

use minigc_compiler::codegen::CodegeGeneratorContext;
use minigc_compiler::compile_expr;

#[test]
fn let_variable_usage() {
    #[compile_expr]
    mod test {
        fn main(a: i64, b: i64) -> i64 {
            let x: i64 = a + 3;
            x + b
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn2::<i64, i64, i64>("main");
    assert_eq!(main_fn(31, 8), 42);
}

#[test]
fn let_variable_scoping() {
    #[compile_expr]
    mod test {
        fn main(a: i64, b: i64) -> i64 {
            let x: i64 = a + 3;
            let y: i64 = {
                let x: i64 = b + 3;
                x + 1
            };
            x + y
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn2::<i64, i64, i64>("main");
    assert_eq!(main_fn(17, 18), 42);
}

#[test]
fn let_mut_simple() {
    #[compile_expr]
    mod test {
        fn main(a: i64) -> i64 {
            let mut x: i64 = a + 1;
            x = x + 5;
            x
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn1::<i64, i64>("main");
    assert_eq!(main_fn(36), 42);
}
