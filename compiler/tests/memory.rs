//! Memory operations.

use minigc_compiler::codegen::CodegeGeneratorContext;
use minigc_compiler::compile_expr;

#[test]
fn empty_memory() {
    #[compile_expr]
    mod test {
        fn main() -> i64 {
            __minigc_memory_size()
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn0::<i64>("main");
    assert_eq!(main_fn(), 0);
}

#[test]
fn allocations() {
    #[compile_expr]
    mod test {
        fn main() -> i64 {
            let x: i64 = __minigc_alloc(7);
            let x: i64 = __minigc_alloc(2);
            __minigc_memory_size()
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn0::<i64>("main");
    assert_eq!(main_fn(), 16);
}
