pub mod execution;
mod names;
pub mod syntax;

pub use minigc_syntax_derive::compile_expr;

#[cfg(test)]
mod tests {
    use minigc_syntax_derive::compile_expr_crate;

    use crate::execution::CodegeGeneratorContext;

    #[test]
    fn simple_test() {
        #[compile_expr_crate]
        mod test {
            fn main(a: i64, b: i64) -> i64 {
                a + b + 4
            }
        }
        test.check().unwrap();
        let ctx = CodegeGeneratorContext::default();
        let main_mod = ctx.generate_module("main", test);
        let main_fn = main_mod.get_fn2::<i64, i64, i64>("main");
        assert_eq!(main_fn(30, 8), 42);
    }

    #[test]
    fn fnc_call() {
        #[compile_expr_crate]
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
        let main_mod = ctx.generate_module("main", test);
        let main_fn = main_mod.get_fn3::<i64, i64, i64, i64>("main");
        assert_eq!(main_fn(5, 15, 22), 42);
    }

    #[test]
    fn abs() {
        #[compile_expr_crate]
        mod test {
            fn abs(x: i64) -> i64 {
                if x <= -1 { -x } else { x }
            }
        }
        test.check().unwrap();
        let ctx = CodegeGeneratorContext::default();
        let main_mod = ctx.generate_module("main", test);
        let abs_fn = main_mod.get_fn1::<i64, i64>("abs");
        assert_eq!(abs_fn(42), 42);
        assert_eq!(abs_fn(-42), 42);
        assert_eq!(abs_fn(0), 0);
    }
}
