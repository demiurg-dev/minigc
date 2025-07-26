use std::fmt::Display;

use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote};
use syn::parse::Nothing;
use syn::{
    BinOp, Block, Error, Expr, ExprPath, FnArg, Ident, Item, ItemMod, Lit, Pat, Stmt, Type, UnOp, parse_macro_input,
};

#[proc_macro_attribute]
pub fn compile_expr_crate(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let this = quote! { crate };
    compile_expr_internal(attrs, item, this)
}

#[proc_macro_attribute]
pub fn compile_expr(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let this = quote! { ::minigc_compiler };
    compile_expr_internal(attrs, item, this)
}

macro_rules! assert_parse {
    ($cond:expr, $tokens:expr, $msg:expr) => {
        if !$cond {
            return error($tokens, $msg);
        }
    };
}

fn compile_expr_internal(attrs: TokenStream, item: TokenStream, this: TokenStream2) -> TokenStream {
    let _ = parse_macro_input!(attrs as Nothing);
    let top_mod: ItemMod = parse_macro_input!(item as ItemMod);

    let content = match top_mod.content {
        Some((_, content)) => content,
        None => {
            return Error::new_spanned(&top_mod.ident, "expected module with body")
                .into_compile_error()
                .into();
        }
    };
    let ident = top_mod.ident;
    let top_builder = quote! { top_builder };

    let mut out_items = Vec::new();
    for item in content {
        match item {
            Item::Fn(fnc) => {
                // TODO: Ignore extra items
                let name = &fnc.sig.ident.to_string();
                let mut tmp_items = Vec::new();
                let params = quote! { params };
                for param in fnc.sig.inputs.iter() {
                    let (name, ty) = match param {
                        FnArg::Receiver(_) => {
                            return Error::new_spanned(param, "self not supported in function parameters")
                                .into_compile_error()
                                .into();
                        }
                        FnArg::Typed(ty) => {
                            let name = match ty.pat.as_ref() {
                                syn::Pat::Ident(pat_ident) => pat_ident.ident.to_string(),
                                _ => {
                                    return Error::new_spanned(ty, "only named parameters are supported")
                                        .into_compile_error()
                                        .into();
                                }
                            };
                            let ty = parse_type(&ty.ty, &this);
                            (name, ty)
                        }
                    };
                    tmp_items.push(quote! { #params.push(#this::syntax::FncParam {
                        name: #name.to_string(),
                        ty: #ty,
                    }); });
                }
                let retty = match &fnc.sig.output {
                    syn::ReturnType::Default => quote! { $this::syntax::Type::Unit },
                    syn::ReturnType::Type(_, ty) => parse_type(ty, &this),
                };
                let body = parse_block(&fnc.block, &this);
                out_items.push(quote! {
                    {
                        let mut #params = ::std::vec::Vec::new();
                        #(#tmp_items)*
                        #top_builder.fncs.insert(#name.to_string(),
                            #this::syntax::Fnc {
                                ty: #this::syntax::FncType {
                                    params: #params.into_boxed_slice(),
                                    ret: #retty
                                },
                                body: #body,
                            }
                        );
                    }
                });
            }
            Item::Struct(strct) => {
                // TODO: Error on extra items
                let name = &strct.ident.to_string();
                let mut tmp_items = Vec::new();
                let fields = quote! { fields };
                for item in strct.fields.iter() {
                    let ty = parse_type(&item.ty, &this);
                    let name = item.ident.as_ref().unwrap().to_string();
                    tmp_items.push(quote! { #fields.push(#this::syntax::StructField {
                        name: #name.to_string(),
                        ty: #ty,
                    }); });
                }

                out_items.push(quote! {
                    {
                        let mut #fields = ::std::vec::Vec::new();
                        #(#tmp_items)*
                        #top_builder.structs.insert(#name.to_string(),
                            #this::syntax::StructType {
                                fields: #fields.into_boxed_slice(),
                            }
                        );
                    }
                });
            }
            _ => {
                return Error::new_spanned(&item, "only functions and structs are supported")
                    .into_compile_error()
                    .into();
            }
        }
    }

    quote! {
        let #ident = {
            let mut #top_builder = #this::syntax::Top::default();
            #(#out_items)*
            #top_builder
        };
    }
    .into()
}

fn parse_type(ty: &Type, this: &TokenStream2) -> TokenStream2 {
    match ty {
        Type::Path(path) => {
            // TODO: Ignore extra items
            let item = path.path.segments.iter().collect_vec();
            assert_eq!(item.len(), 1);
            match item[0].ident.to_string().as_str() {
                "u8" => quote! { #this::syntax::Type::Int { size: #this::syntax::IntSize::I8, signed: false } },
                "i8" => quote! { #this::syntax::Type::Int { size: #this::syntax::IntSize::I8, signed: true } },
                "u16" => quote! { #this::syntax::Type::Int { size: #this::syntax::IntSize::I16, signed: false } },
                "i16" => quote! { #this::syntax::Type::Int { size: #this::syntax::IntSize::I16, signed: true } },
                "u32" => quote! { #this::syntax::Type::Int { size: #this::syntax::IntSize::I32, signed: false } },
                "i32" => quote! { #this::syntax::Type::Int { size: #this::syntax::IntSize::I32, signed: true } },
                "u64" => quote! { #this::syntax::Type::Int { size: #this::syntax::IntSize::I64, signed: false } },
                "i64" => quote! { #this::syntax::Type::Int { size: #this::syntax::IntSize::I64, signed: true } },
                "bool" => quote! { #this::syntax::Type::Bool },
                _ => {
                    let name = item[0].ident.to_string();
                    quote! { #this::syntax::Type::Name(#name.to_string()) }
                }
            }
        }
        _ => unimplemented!("Unsupported type: {ty:?}"),
    }
}

fn parse_stmt(stmt: &Stmt, this: &TokenStream2) -> TokenStream2 {
    match stmt {
        Stmt::Expr(expr, _semi) => parse_expr(expr, this),
        Stmt::Local(local) => {
            let (id, is_mut, ty) = match &local.pat {
                Pat::Ident(_) => return error(local, "type annotation needed"),
                Pat::Type(ty) => {
                    assert_parse!(ty.attrs.is_empty(), &ty, "attributes not supported");
                    let (id, is_mut) = match &*ty.pat {
                        Pat::Ident(ident) => {
                            assert_parse!(ident.attrs.is_empty(), &ident, "attributes not supported");
                            assert_parse!(ident.by_ref.is_none(), ident, "ref not supported");
                            assert_parse!(ident.subpat.is_none(), ident, "sub-pattern not supported");
                            let is_mut = ident.mutability.is_some();
                            (ident.ident.to_string(), is_mut)
                        }
                        _ => return error(&local.pat, "unsupported let binding"),
                    };
                    let ty = parse_type(&ty.ty, this);
                    (id, is_mut, ty)
                }
                _ => return error(&local.pat, "unsupported let binding"),
            };
            assert_parse!(local.attrs.is_empty(), &local.attrs.first().unwrap(), "attributes not supported");
            match &local.init {
                Some(init) => {
                    assert_parse!(
                        init.diverge.is_none(),
                        &init.diverge.as_ref().unwrap().1,
                        "diverge expression not supported"
                    );
                    let expr = parse_expr(&init.expr, this);
                    quote! {
                        #this::syntax::Expr::Let {
                            name: #id.to_string(),
                            ty: #ty,
                            is_mut: #is_mut,
                            rhs: ::std::boxed::Box::new(#expr)
                        }
                    }
                }
                None => error(local, "initialization expression is required"),
            }
        }
        _ => Error::new_spanned(stmt, "unsupported statement type").into_compile_error(),
    }
}

fn parse_expr(expr: &Expr, this: &TokenStream2) -> TokenStream2 {
    match expr {
        Expr::Lit(lit) => match &lit.lit {
            Lit::Int(int) => {
                let value = int.base10_parse::<i64>().unwrap();
                quote! { #this::syntax::Expr::Const(#value) }
            }
            _ => unimplemented!(),
        },
        Expr::Path(path) => {
            let ident = parse_path_to_ident(path).to_string();
            quote! { #this::syntax::Expr::Var(#ident.to_string()) }
        }
        Expr::Binary(bin) => {
            let op = match &bin.op {
                BinOp::Add(_) => quote! { #this::syntax::BinOp::Add },
                BinOp::Sub(_) => quote! { #this::syntax::BinOp::Sub },
                BinOp::Mul(_) => quote! { #this::syntax::BinOp::Mul },
                BinOp::Le(_) => quote! { #this::syntax::BinOp::Leq },
                _ => unimplemented!(),
            };
            let left = parse_expr(&bin.left, this);
            let right = parse_expr(&bin.right, this);
            quote! { #this::syntax::Expr::BinOp {
                op: #op,
                left: ::std::boxed::Box::new(#left),
                right: ::std::boxed::Box::new(#right)
            }}
        }
        Expr::Unary(un) => {
            let op = match &un.op {
                UnOp::Neg(_) => quote! { #this::syntax::UnOp::Neg },
                _ => unimplemented!(),
            };
            let expr = parse_expr(&un.expr, this);
            quote! {
                #this::syntax::Expr::UnOp {
                    op: #op,
                    expr: ::std::boxed::Box::new(#expr),
                }
            }
        }
        Expr::If(ite) => {
            let cond = parse_expr(&ite.cond, this);
            let cond = quote! { ::std::boxed::Box::new(#cond) };
            let then_branch = parse_block(&ite.then_branch, this);
            let then_branch = quote! { ::std::boxed::Box::new(#then_branch) };
            let else_branch = ite
                .else_branch
                .as_ref()
                .map(|(_, else_branch)| {
                    let else_branch = parse_expr(else_branch, this);
                    quote! {
                        Some(::std::boxed::Box::new(#else_branch))
                    }
                })
                .unwrap_or_else(|| quote! { None });
            quote! {
                #this::syntax::Expr::Ite { cond: #cond, then_branch: #then_branch, else_branch: #else_branch }
            }
        }
        Expr::While(whl) => {
            assert_parse!(whl.attrs.is_empty(), whl.attrs.first(), "attributes not supported");
            assert_parse!(whl.label.is_none(), whl.label.as_ref().unwrap(), "label not supported");
            let cond = parse_expr(&whl.cond, this);
            let body = parse_block(&whl.body, this);
            quote! {
                #this::syntax::Expr::While {
                    cond: ::std::boxed::Box::new(#cond),
                    body: ::std::boxed::Box::new(#body),
                }
            }
        }
        Expr::Call(call) => {
            let name = match &*call.func {
                Expr::Path(path) => parse_path_to_ident(path).to_string(),
                _ => unimplemented!("Only call by function name supported"),
            };
            let args = call
                .args
                .iter()
                .map(|arg| parse_expr(arg, this))
                .collect_vec();
            quote! {
                #this::syntax::Expr::Call {
                    name: #name.to_string(),
                    args: ::std::vec![#(#args),*].into_boxed_slice(),
                }
            }
        }
        Expr::Assign(assign) => {
            assert_parse!(assign.attrs.is_empty(), assign.attrs.first(), "attributes not supported");
            let name = match &*assign.left {
                Expr::Path(path) => parse_path_to_ident(path).to_string(),
                _ => return error(&assign.left, "expected identifier"),
            };
            let expr = parse_expr(&assign.right, this);
            quote! {
                #this::syntax::Expr::Assign { name: #name.to_string(), expr: ::std::boxed::Box::new(#expr) }
            }
        }
        Expr::Block(block) => parse_block(&block.block, this),
        _ => unimplemented!("Unsupported expression type: {expr:?}"),
    }
}

fn parse_path_to_ident(path: &ExprPath) -> &Ident {
    let segments = path.path.segments.iter().collect_vec();
    assert_eq!(segments.len(), 1);
    &segments[0].ident
}

fn parse_block(block: &Block, this: &TokenStream2) -> TokenStream2 {
    let stmts = block.stmts.iter().map(|stmt| parse_stmt(stmt, this));
    quote! {
        #this::syntax::Expr::Block(
            ::std::vec![#(#stmts),*].into_boxed_slice(),
        )
    }
}

fn error<T, E>(tokens: T, msg: E) -> TokenStream2
where
    T: ToTokens,
    E: Display,
{
    Error::new_spanned(tokens, msg).into_compile_error()
}
