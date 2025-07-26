//! Let-expressions and variable usage.

use minigc_compiler::compile_expr;
use minigc_compiler::execution::CodegeGeneratorContext;

#[test]
fn let_variable_usage() {
    #[compile_expr]
    mod test {
        fn main(a: i64, b: i64) -> i64 {
            let x: i64 = a + 3;
            b + 2
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn2::<i64, i64, i64>("main");
    assert_eq!(main_fn(30, 8), 42);
}
