use hashbrown::HashMap;
use inkwell::IntPredicate;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{AggregateValue, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue};
use itertools::Itertools;

use crate::names::{Name, Names};
use crate::runtime::Runtime;
use crate::syntax::{BinOp, Expr, FncType, IntSize, Top, Type, UnOp};

type NameCtx<'a> = im::HashMap<Name, (BasicValueEnum<'a>, Option<BasicTypeEnum<'a>>)>;

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
        self.generate_structs();
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
            .map(|param| self.generate_value_type(&param.ty).into())
            .collect_vec();
        match &ty.ret {
            Type::Unit => self.context.void_type().fn_type(&param_types, false),
            _ => {
                let generic = self.generate_value_type(&ty.ret);
                generic.fn_type(&param_types, false)
            }
        }
    }

    fn generate_value_type(&self, ty: &Type) -> BasicTypeEnum<'a> {
        match ty {
            Type::Unit => unreachable!("Value type cannot be unit/void in LLVM"),
            Type::Int { size, .. } => match size {
                IntSize::I8 => self.context.i8_type(),
                IntSize::I16 => self.context.i16_type(),
                IntSize::I32 => self.context.i32_type(),
                IntSize::I64 => self.context.i64_type(),
            }
            .into(),
            Type::Name(name) => self.context.get_struct_type(name).unwrap().into(),
            _ => unimplemented!(),
        }
    }

    fn generate_fncs(&mut self) {
        for (name, fnc) in &self.top.fncs {
            let fnc_def = self.module.get_function(name).unwrap();
            let entry = self.context.append_basic_block(fnc_def, "entry");
            self.builder.position_at_end(entry);

            let mut names = Names::default();
            let mut ctx: NameCtx = Default::default();
            for (i, param) in fnc.ty.params.iter().enumerate() {
                let name = self.names.get_str(&param.name);
                let param = fnc_def.get_nth_param(i as u32).unwrap();
                let _ = ctx.insert(name, (param, None));
            }

            self.current_fnc = Some(fnc_def);
            let ret = self.generate_expr(&fnc.body, Some("ret"), &ctx, &mut names);
            self.builder
                .build_return(ret.as_ref().map(|x| x as &dyn BasicValue<'_>))
                .unwrap();
            self.current_fnc = None;
        }
    }

    fn generate_structs(&self) {
        for (name, sd) in &self.top.structs {
            let fields = sd
                .fields
                .iter()
                .map(|f| self.generate_value_type(&f.ty))
                .collect_vec();
            let st = self.context.opaque_struct_type(name);
            st.set_body(&fields, false);
        }
    }

    fn generate_expr(
        &self,
        expr: &Expr,
        name: Option<&str>,
        ctx: &NameCtx<'a>,
        names: &mut Names,
    ) -> Option<BasicValueEnum<'a>> {
        match expr {
            Expr::Const(val) => {
                // TODO: Need to have type information here
                Some(self.context.i64_type().const_int(*val as u64, false).into())
            }
            Expr::Var(name) => {
                let (val, mut_type) = *ctx.get(name.as_str()).unwrap();
                Some(match mut_type {
                    Some(ty) => self
                        .builder
                        .build_load(ty, val.into_pointer_value(), name)
                        .unwrap(),
                    None => val,
                })
            }
            Expr::Struct { name: s_name, exprs } => {
                // TODO: What is struct has no fields (this should be equivalent to unit type)
                assert!(!exprs.is_empty());
                let fields: HashMap<_, _> = self
                    .top
                    .structs
                    .get(s_name)
                    .unwrap()
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, fld)| (&fld.name, i as u32))
                    .collect();
                let strct = self.context.get_struct_type(s_name).unwrap();
                let mut val = strct.get_poison().as_aggregate_value_enum();
                let last_idx = exprs.len() - 1;
                for (i, (f_name, expr)) in exprs.iter().enumerate() {
                    let idx = fields.get(f_name).unwrap();
                    let e_name = format!("{s_name}.{f_name}");
                    let expr = self.generate_expr(expr, Some(&e_name), ctx, names).unwrap();
                    let name = if i == last_idx { name.unwrap_or_default() } else { "" };
                    val = self
                        .builder
                        .build_insert_value(val, expr, *idx, name)
                        .unwrap();
                }
                Some(val.as_basic_value_enum())
                /*let mut field_exprs: HashMap<_, _> = HashMap::from_iter(exprs.iter().map(|(name, expr)| (name, expr)));
                let fields = self.top.structs.get(name).unwrap().fields.iter().map(|fld| {
                    let expr = field_exprs.remove(&fld.name).unwrap();
                    let name = format!("{name}.{}", &fld.name);
                    self.generate_expr(expr, Some(&name), ctx, names).unwrap()
                }).collect_vec();
                assert!(field_exprs.is_empty());
                Some(self.context.get_struct_type(name).unwrap().const_named_struct(&fields).as_basic_value_enum())*/
            }
            Expr::StructField { base, name: f_name } => {
                let (val, _is_mut) = *ctx.get(base.as_str()).unwrap();
                let strct = val.into_struct_value();
                let idx = self
                    .top
                    .structs
                    .get(strct.get_type().get_name().unwrap().to_str().unwrap())
                    .unwrap()
                    .fields
                    .iter()
                    .find_position(|fld| &fld.name == f_name)
                    .unwrap()
                    .0 as u32;
                let name = name.unwrap_or(f_name);
                Some(self.builder.build_extract_value(strct, idx, name).unwrap())
            }
            Expr::BinOp { op, left, right } => match op {
                BinOp::Add => {
                    let lhs = self
                        .generate_expr(left, Some("lhs"), ctx, names)
                        .unwrap()
                        .into_int_value();
                    let rhs = self
                        .generate_expr(right, Some("rhs"), ctx, names)
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
                        .generate_expr(left, Some("lhs"), ctx, names)
                        .unwrap()
                        .into_int_value();
                    let rhs = self
                        .generate_expr(right, Some("rhs"), ctx, names)
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
                        .generate_expr(left, Some("lhs"), ctx, names)
                        .unwrap()
                        .into_int_value();
                    let rhs = self
                        .generate_expr(right, Some("rhs"), ctx, names)
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
                        .generate_expr(left, Some("lhs"), ctx, names)
                        .unwrap()
                        .into_int_value();
                    let rhs = self
                        .generate_expr(right, Some("rhs"), ctx, names)
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
                        .generate_expr(expr, name, ctx, names)
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
                    .generate_expr(cond, Some("cond"), ctx, names)
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
                let then_value = self.generate_expr(then_branch, Some("then"), ctx, names);
                self.builder.build_unconditional_branch(end_block).unwrap();

                self.builder.position_at_end(else_block);
                let else_value = else_branch
                    .as_ref()
                    .and_then(|else_branch| self.generate_expr(else_branch, Some("ctx"), ctx, names));
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
            Expr::While { cond, body } => {
                let curr_fnc = self.current_fnc.unwrap();

                let cond_block = self.context.append_basic_block(curr_fnc, "while_cond");
                let body_block = self.context.append_basic_block(curr_fnc, "while_body");
                let end_block = self.context.append_basic_block(curr_fnc, "while_end");

                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(cond_block);
                let cond = self
                    .generate_expr(cond, Some("cond"), ctx, names)
                    .unwrap()
                    .into_int_value();
                self.builder
                    .build_conditional_branch(cond, body_block, end_block)
                    .unwrap();

                self.builder.position_at_end(body_block);
                assert!(self.generate_expr(body, None, ctx, names).is_none());
                self.builder.build_unconditional_branch(cond_block).unwrap();

                self.builder.position_at_end(end_block);
                None
            }
            Expr::Let { .. } => unreachable!("standalone let"),
            Expr::Assign { name, expr } => {
                if let Some(val) = self.generate_expr(expr, Some(name), ctx, names) {
                    let name = names.get_str(name);
                    let dst = ctx.get(&name).unwrap().0.into_pointer_value();
                    self.builder.build_store(dst, val).unwrap();
                }
                None
            }
            Expr::Call { name: fname, args } => {
                // TODO: Filter any potential unit type
                let args = args
                    .iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        BasicMetadataValueEnum::from(
                            self.generate_expr(arg, Some(format!("arg_{i}").as_str()), ctx, names)
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
            Expr::Block(exprs) => {
                let mut ctx = ctx.clone();
                assert!(!exprs.is_empty(), "empty exprs not supported yet");
                let idx_last = exprs.len() - 1;
                for (i, expr) in exprs.iter().enumerate() {
                    let name = if i == idx_last { name } else { None };
                    match expr {
                        Expr::Let { name, rhs, is_mut, ty } => {
                            if let Some(val) = self.generate_expr(rhs, Some(name), &ctx, names) {
                                let entry = if *is_mut {
                                    let ty = self.generate_value_type(ty);
                                    let alloc = self.builder.build_alloca(ty, name).unwrap();
                                    self.builder.build_store(alloc, val).unwrap();
                                    (alloc.as_basic_value_enum(), Some(ty))
                                } else {
                                    (val, None)
                                };
                                ctx.insert(names.get_str(name), entry);
                            }
                        }
                        _ => {
                            let val = self.generate_expr(expr, name, &ctx, names);
                            if i == idx_last {
                                return val;
                            }
                        }
                    }
                }
                unreachable!()
            }
            Expr::Unit => todo!(),
        }
    }
}
