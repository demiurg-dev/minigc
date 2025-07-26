//! Let-expressions and variable usage.

use minigc_compiler::compile_expr;
use minigc_compiler::execution::CodegeGeneratorContext;

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
