use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse::Nothing;
use syn::{BinOp, Block, Error, Expr, FnArg, Item, ItemMod, Lit, Stmt, Type, parse_macro_input};

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

    /*for item in top_mod.items {

    }*/

    // This is a placeholder implementation.
    // In a real implementation, you would parse the `attr` and `item` tokens,
    // perform some transformations, and return the modified item.

    // For now, we just return the item unchanged.
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
        Stmt::Local(_local) => {
            todo!()
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
            let segments = path.path.segments.iter().collect_vec();
            assert_eq!(segments.len(), 1);
            let ident = segments[0].ident.to_string();
            quote! { #this::syntax::Expr::Var(#ident.to_string()) }
        }
        Expr::Binary(bin) => {
            let op = match &bin.op {
                BinOp::Add(_) => quote! { #this::syntax::BinOp::Add },
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
        Expr::Block(block) => parse_block(&block.block, this),
        _ => unimplemented!("Unsupported expression type: {expr:?}"),
    }
}

fn parse_block(block: &Block, this: &TokenStream2) -> TokenStream2 {
    let stmts = block.stmts.iter().map(|stmt| parse_stmt(stmt, this));
    quote! {
        #this::syntax::Expr::Block(
            ::std::vec![#(#stmts),*].into_boxed_slice(),
        )
    }
}
