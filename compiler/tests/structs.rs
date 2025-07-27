//! Struct definitions and operations.

use minigc_compiler::codegen::CodegeGeneratorContext;
use minigc_compiler::compile_expr;

#[test]
fn struct_simple() {
    #[compile_expr]
    mod test {
        struct Point {
            x: i64,
            y: i64,
        }

        fn abs(x: i64) -> i64 {
            if x <= 0 { -x } else { x }
        }

        fn point_new(x: i64, y: i64) -> Point {
            Point { x, y }
        }

        fn point_man_dist(p1: Point, p2: Point) -> i64 {
            let dx: i64 = abs(p1.x - p2.x);
            let dy: i64 = abs(p1.y - p2.y);
            dx + dy
        }

        fn main(x1: i64, y1: i64, x2: i64, y2: i64) -> i64 {
            let p1: Point = point_new(x1, y1);
            let p2: Point = point_new(x2, y2);
            point_man_dist(p1, p2)
        }
    }
    test.check().unwrap();
    let ctx = CodegeGeneratorContext::default();
    let main_mod = ctx.generate_module("main", test).init_runtime();
    let main_fn = main_mod.get_fn4::<i64, i64, i64, i64, i64>("main");
    assert_eq!(main_fn(10, 15, 7, 21), 9);
}
