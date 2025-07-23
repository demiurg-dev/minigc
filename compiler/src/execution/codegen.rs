use std::collections::HashMap;

use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, FunctionType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use itertools::Itertools;

use crate::names::{Name, Names};
use crate::syntax::{BinOp, Expr, FncType, IntSize, Top, Type};

type NameCtx<'a> = im::HashMap<Name, BasicValueEnum<'a>>;

pub struct CodegeGeneratorContext {
    context: Context,
}

impl CodegeGeneratorContext {
    pub fn generate_module<'a>(&'a self, name: &'a str, program: Top) -> ExecutionModule<'a> {
        ExecutionModule::new(&self.context, name, program)
    }
}

impl Default for CodegeGeneratorContext {
    fn default() -> Self {
        let context = Context::create();
        Self { context }
    }
}

pub struct ExecutionModule<'a> {
    name: &'a str,
    _module: Module<'a>,
    main: extern "C" fn(i64, i64) -> i64,
}

impl<'a> std::fmt::Debug for ExecutionModule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExecutionModule")
            .field("name", &self.name)
            .finish()
    }
}

impl<'a> ExecutionModule<'a> {
    fn new(context: &'a Context, name: &'a str, top: Top) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        let mut generator = Generator::new(context, &module, builder, top);
        generator.generate_top();

        eprintln!("{}", module.print_to_string().to_string_lossy());
        if let Err(err) = module.verify() {
            panic!("Verify error: {err}");
        }

        let ee = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        let main_fn: extern "C" fn(i64, i64) -> i64 =
            unsafe { std::mem::transmute(ee.get_function_address("main").unwrap()) };

        Self { name, _module: module, main: main_fn }
    }

    pub fn call(&self, x: i64, y: i64) -> i64 {
        (self.main)(x, y)
    }
}

struct Generator<'a, 'm> {
    context: &'a Context,
    module: &'m Module<'a>,
    builder: Builder<'a>,
    fncs: HashMap<Name, FncDef<'a>>,
    names: Names,
    top: Top,
}

impl<'a, 'm> Generator<'a, 'm> {
    fn new(context: &'a Context, module: &'m Module<'a>, builder: Builder<'a>, top: Top) -> Self {
        Self { context, module, builder, fncs: HashMap::new(), names: Names::default(), top }
    }

    fn generate_top(&mut self) {
        self.generate_fnc_defs();
        self.generate_fncs();
    }

    fn generate_fnc_defs(&mut self) {
        let mut fncs = HashMap::new();
        for (name, fnc) in &self.top.fncs {
            let fnc_ty = self.generate_fnc_type(&fnc.ty);
            let fnc_value = self.module.add_function(name.as_str(), fnc_ty, None);
            let name = self.names.get_str(name.as_str());
            fncs.insert(name, FncDef { value: fnc_value, _ty: fnc_ty });
        }
        self.fncs = fncs;
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
        for ((_, fnc_def), (_, fnc)) in self.fncs.iter().zip(&self.top.fncs) {
            let entry = self.context.append_basic_block(fnc_def.value, "entry");
            self.builder.position_at_end(entry);

            let mut ctx: NameCtx = Default::default();
            for (i, param) in fnc.ty.params.iter().enumerate() {
                let name = self.names.get_str(&param.name);
                let param = fnc_def.value.get_nth_param(i as u32).unwrap();
                let _ = ctx.insert(name, param);
            }

            let ret = self.generate_expr(&fnc.body, Some("ret"), &ctx);
            self.builder
                .build_return(ret.as_ref().map(|x| x as &dyn BasicValue<'_>))
                .unwrap();
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
            },
            Expr::Let { .. } => todo!(),
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

struct FncDef<'a> {
    value: FunctionValue<'a>,
    _ty: FunctionType<'a>,
}
