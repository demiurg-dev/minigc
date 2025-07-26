//! Struct definitions and operations.

use minigc_compiler::codegen::CodegeGeneratorContext;
use minigc_compiler::compile_expr;

#[test]
fn while_simple() {
    #[compile_expr]
    mod test {
        struct Point {
            x: i64,
            y: i64,
        }
        
        fn main(x: i64, y: Point) -> i64 {
            x
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn1::<i64, i64>("main");
    assert_eq!(main_fn(42), 42);
}