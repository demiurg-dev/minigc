use inkwell::IntPredicate;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, FunctionType};
use inkwell::values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue};
use itertools::Itertools;

use crate::names::{Name, Names};
use crate::runtime::Runtime;
use crate::syntax::{BinOp, Expr, FncType, IntSize, Top, Type, UnOp};

type NameCtx<'a> = im::HashMap<Name, BasicValueEnum<'a>>;

pub struct CodegeGeneratorContext {
    context: Context,
}

impl CodegeGeneratorContext {
    pub fn generate_module<'a>(&'a self, name: &str, program: Top) -> GeneratedModule<'a> {
        GeneratedModule::new(&self.context, name, program)
    }
}

impl Default for CodegeGeneratorContext {
    fn default() -> Self {
        let context = Context::create();
        Self { context }
    }
}

pub struct GeneratedModule<'a> {
    module: Module<'a>,
}

impl<'a> std::fmt::Debug for GeneratedModule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExecutionModule")
            .field("name", &self.module.get_name().to_str().unwrap())
            .finish()
    }
}

impl<'a> GeneratedModule<'a> {
    fn new(context: &'a Context, name: &str, top: Top) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        let mut generator = Generator::new(context, &module, builder, top);
        generator.generate_top();

        eprintln!("{}", module.print_to_string().to_string_lossy());
        if let Err(err) = module.verify() {
            panic!("Verify error: {err}");
        }

        Self { module }
    }

    pub fn init_runtime(self) -> Runtime<'a> {
        Runtime::new(self.module)
    }
}

struct Generator<'a, 'm> {
    context: &'a Context,
    module: &'m Module<'a>,
    builder: Builder<'a>,
    current_fnc: Option<FunctionValue<'a>>,
    names: Names,
    top: Top,
}

impl<'a, 'm> Generator<'a, 'm> {
    fn new(context: &'a Context, module: &'m Module<'a>, builder: Builder<'a>, top: Top) -> Self {
        Self { context, module, builder, current_fnc: None, names: Names::default(), top }
    }

    fn generate_top(&mut self) {
        self.generate_fnc_defs();
        self.generate_fncs();
    }

    fn generate_fnc_defs(&mut self) {
        for (name, fnc) in &self.top.fncs {
            let fnc_ty = self.generate_fnc_type(&fnc.ty);
            self.module.add_function(name.as_str(), fnc_ty, None);
        }
    }

    fn generate_fnc_type(&self, ty: &FncType) -> FunctionType<'a> {
        // TODO: Handle (or restrict during checking) unit param type
        let param_types = ty
            .params
            .iter()
            .map(|param| self.generate_value_type(&param.ty))
            .collect_vec();
        match &ty.ret {
            Type::Unit => self.context.void_type().fn_type(&param_types, false),
            _ => {
                let generic = self.generate_value_type(&ty.ret);
                match generic {
                    BasicMetadataTypeEnum::IntType(it) => it.fn_type(&param_types, false),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn generate_value_type(&self, ty: &Type) -> BasicMetadataTypeEnum<'a> {
        match ty {
            Type::Unit => unreachable!("Value type cannot be unit/void in LLVM"),
            Type::Int { size, .. } => match size {
                IntSize::I8 => self.context.i8_type(),
                IntSize::I16 => self.context.i16_type(),
                IntSize::I32 => self.context.i32_type(),
                IntSize::I64 => self.context.i64_type(),
            }
            .into(),
            _ => unimplemented!(),
        }
    }

    fn generate_fncs(&mut self) {
        for (name, fnc) in &self.top.fncs {
            let fnc_def = self.module.get_function(name).unwrap();
            let entry = self.context.append_basic_block(fnc_def, "entry");
            self.builder.position_at_end(entry);

            let mut ctx: NameCtx = Default::default();
            for (i, param) in fnc.ty.params.iter().enumerate() {
                let name = self.names.get_str(&param.name);
                let param = fnc_def.get_nth_param(i as u32).unwrap();
                let _ = ctx.insert(name, param);
            }

            self.current_fnc = Some(fnc_def);
            let ret = self.generate_expr(&fnc.body, Some("ret"), &ctx);
            self.builder
                .build_return(ret.as_ref().map(|x| x as &dyn BasicValue<'_>))
                .unwrap();
            self.current_fnc = None;
        }
    }

    fn generate_expr(&self, expr: &Expr, name: Option<&str>, ctx: &NameCtx<'a>) -> Option<BasicValueEnum<'a>> {
        match expr {
            Expr::Const(val) => {
                // TODO: Need to have type information here
                Some(self.context.i64_type().const_int(*val as u64, false).into())
            }
            Expr::Var(name) => Some(*ctx.get(name.as_str()).unwrap()),
            Expr::BinOp { op, left, right } => match op {
                BinOp::Add => {
                    let lhs = self
                        .generate_expr(left, Some("lhs"), ctx)
                        .unwrap()
                        .into_int_value();
                    let rhs = self
                        .generate_expr(right, Some("rhs"), ctx)
                        .unwrap()
                        .into_int_value();
                    Some(
                        self.builder
                            .build_int_add(lhs, rhs, name.unwrap_or("add"))
                            .unwrap()
                            .into(),
                    )
                }
                BinOp::Sub => {
                    let lhs = self
                        .generate_expr(left, Some("lhs"), ctx)
                        .unwrap()
                        .into_int_value();
                    let rhs = self
                        .generate_expr(right, Some("rhs"), ctx)
                        .unwrap()
                        .into_int_value();
                    Some(
                        self.builder
                            .build_int_sub(lhs, rhs, name.unwrap_or("sub"))
                            .unwrap()
                            .into(),
                    )
                }
                BinOp::Mul => {
                    let lhs = self
                        .generate_expr(left, Some("lhs"), ctx)
                        .unwrap()
                        .into_int_value();
                    let rhs = self
                        .generate_expr(right, Some("rhs"), ctx)
                        .unwrap()
                        .into_int_value();
                    Some(
                        self.builder
                            .build_int_mul(lhs, rhs, name.unwrap_or("mul"))
                            .unwrap()
                            .into(),
                    )
                }
                BinOp::Leq => {
                    let lhs = self
                        .generate_expr(left, Some("lhs"), ctx)
                        .unwrap()
                        .into_int_value();
                    let rhs = self
                        .generate_expr(right, Some("rhs"), ctx)
                        .unwrap()
                        .into_int_value();
                    Some(
                        // TODO: We need type information to build correct predicate (signed/unsigned)
                        self.builder
                            .build_int_compare(IntPredicate::SLE, lhs, rhs, name.unwrap_or("leq"))
                            .unwrap()
                            .into(),
                    )
                }
            },
            Expr::UnOp { op, expr } => match op {
                UnOp::Neg => {
                    let expr = self
                        .generate_expr(expr, name, ctx)
                        .unwrap()
                        .into_int_value();
                    Some(
                        self.builder
                            .build_int_neg(expr, name.unwrap_or("neg"))
                            .unwrap()
                            .as_basic_value_enum(),
                    )
                }
            },
            Expr::Ite { cond, then_branch, else_branch } => {
                let cond = self
                    .generate_expr(cond, Some("cond"), ctx)
                    .unwrap()
                    .into_int_value();
                let curr_fnc = self.current_fnc.unwrap();

                let then_block = self.context.append_basic_block(curr_fnc, "then");
                let else_block = self.context.append_basic_block(curr_fnc, "else");
                let end_block = self.context.append_basic_block(curr_fnc, "end");

                self.builder
                    .build_conditional_branch(cond, then_block, else_block)
                    .unwrap();

                self.builder.position_at_end(then_block);
                let then_value = self.generate_expr(then_branch, Some("then"), ctx);
                self.builder.build_unconditional_branch(end_block).unwrap();

                self.builder.position_at_end(else_block);
                let else_value = else_branch
                    .as_ref()
                    .and_then(|else_branch| self.generate_expr(else_branch, Some("ctx"), ctx));
                self.builder.build_unconditional_branch(end_block).unwrap();

                self.builder.position_at_end(end_block);
                then_value.map(|then_value| {
                    let ty = then_value.get_type();
                    let else_value = else_value.unwrap();
                    let phi = self.builder.build_phi(ty, name.unwrap_or("ite")).unwrap();
                    phi.add_incoming(&[(&then_value, then_block), (&else_value, else_block)]);
                    phi.as_basic_value()
                })
            }
            Expr::Let { .. } => todo!(),
            Expr::Call { name: fname, args } => {
                // TODO: Filter any potential unit type
                let args = args
                    .iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        BasicMetadataValueEnum::from(
                            self.generate_expr(arg, Some(format!("arg_{i}").as_str()), ctx)
                                .unwrap(),
                        )
                    })
                    .collect_vec();
                let fnc = self.module.get_function(fname).unwrap();
                let ty = fnc.get_type();
                let retty = ty.get_return_type();
                let maybe_name = format!("ret_{fname}");
                let name = match retty {
                    Some(_) => name.unwrap_or(&maybe_name),
                    None => "",
                };
                let val = self.builder.build_call(fnc, &args, name).unwrap();
                retty.map(|_| val.try_as_basic_value().unwrap_left())
            }
            Expr::Ref(_expr) => todo!(),
            Expr::Block(exprs) => {
                // TODO: We need type information here (to know whether something is unit)
                // TODO: Handle let differently here
                assert!(!exprs.is_empty(), "empty exprs not supported yet");
                let idx_last = exprs.len() - 1;
                for (i, expr) in exprs.iter().enumerate() {
                    let name = if i == idx_last { name } else { None };
                    let val = self.generate_expr(expr, name, ctx);
                    if i == idx_last {
                        return val;
                    }
                }
                unreachable!()
            }
            Expr::Unit => todo!(),
        }
    }
}
