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
        let ctx = CodegeGeneratorContext::default();
        let main_mod = ctx.generate_module("main", test);
        assert_eq!(main_mod.call(30, 8), 42);
    }

    #[test]
    fn fnc_call() {
        #[compile_expr_crate]
        mod test {
            fn main(x: i64, y: i64, z: i64) -> i64 {
                x + sum(y, x)
            }

            fn sum(x: i64, y: i64) -> i64 {
                x + y
            }
        }
        test.check().unwrap();
    }
}
