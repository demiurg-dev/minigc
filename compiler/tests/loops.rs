//! Loop constructs (while).

use minigc_compiler::codegen::CodegeGeneratorContext;
use minigc_compiler::compile_expr;

#[test]
fn while_simple() {
    #[compile_expr]
    mod test {
        fn main(n: i64) -> i64 {
            let mut res: i64 = 0;
            let mut n: i64 = n;
            while 1 <= n {
                res = res + 2;
                n = n - 1;
            }
            res
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn1::<i64, i64>("main");
    assert_eq!(main_fn(21), 42);
}

#[test]
fn fact() {
    #[compile_expr]
    mod test {
        fn main(n: i64) -> i64 {
            let mut res: i64 = 1;
            let mut i: i64 = 2;
            while i <= n {
                res = res * i;
                i = i + 1;
            }
            res
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn1::<i64, i64>("main");
    assert_eq!(main_fn(-42), 1);
    assert_eq!(main_fn(0), 1);
    assert_eq!(main_fn(1), 1);
    assert_eq!(main_fn(2), 2);
    assert_eq!(main_fn(3), 6);
    assert_eq!(main_fn(5), 120);
}
