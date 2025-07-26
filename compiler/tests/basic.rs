//! Basic expressions and function calls.

use minigc_compiler::codegen::CodegeGeneratorContext;
use minigc_compiler::compile_expr;

#[test]
fn simple_test() {
    #[compile_expr]
    mod test {
        fn main(a: i64, b: i64) -> i64 {
            a + b + 4
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn2::<i64, i64, i64>("main");
    assert_eq!(main_fn(30, 8), 42);
}

#[test]
fn fnc_call() {
    #[compile_expr]
    mod test {
        fn main(x: i64, y: i64, z: i64) -> i64 {
            x + sum(y, z)
        }

        fn sum(x: i64, y: i64) -> i64 {
            x + y
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn3::<i64, i64, i64, i64>("main");
    assert_eq!(main_fn(5, 15, 22), 42);
}

#[test]
fn abs() {
    #[compile_expr]
    mod test {
        fn abs(x: i64) -> i64 {
            if x <= -1 { -x } else { x }
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let abs_fn = main_mod.get_fn1::<i64, i64>("abs");
    assert_eq!(abs_fn(42), 42);
    assert_eq!(abs_fn(-42), 42);
    assert_eq!(abs_fn(0), 0);
}

#[test]
fn fact() {
    // TODO: This would ideally be u64 (when we have better support for it)
    #[compile_expr]
    mod test {
        fn fact(x: i64) -> i64 {
            if x <= 1 { 1 } else { x * fact(x - 1) }
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let fact_fn = main_mod.get_fn1::<i64, i64>("fact");
    assert_eq!(fact_fn(-42), 1);
    assert_eq!(fact_fn(0), 1);
    assert_eq!(fact_fn(1), 1);
    assert_eq!(fact_fn(2), 2);
    assert_eq!(fact_fn(5), 120);
}

#[test]
fn fact_acc() {
    // TODO: This would ideally be u64 (when we have better support for it)
    #[compile_expr]
    mod test {
        fn fact_acc(x: i64, acc: i64) -> i64 {
            if x <= 1 {
                acc
            } else {
                fact_acc(x - 1, acc * x);
            }
        }

        fn fact(x: i64) -> i64 {
            fact_acc(x, 1);
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let fact_fn = main_mod.get_fn1::<i64, i64>("fact");
    assert_eq!(fact_fn(-42), 1);
    assert_eq!(fact_fn(0), 1);
    assert_eq!(fact_fn(1), 1);
    assert_eq!(fact_fn(2), 2);
    assert_eq!(fact_fn(5), 120);
}
