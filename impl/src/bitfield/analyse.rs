use super::{
    config::{Config, ReprKind},
    field_config::{FieldConfig, SkipWhich},
    BitfieldStruct,
};
use crate::errors::CombineError;
use core::convert::TryFrom;
use proc_macro2::{TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use std::collections::HashMap;
use syn::{
    self,
    parse::{Parser, Result},
    punctuated::Punctuated,
    spanned::Spanned as _,
    Expr, ExprLit, Lit, Token,
};

impl TryFrom<(&mut Config, syn::ItemStruct)> for BitfieldStruct {
    type Error = syn::Error;

    fn try_from((config, item_struct): (&mut Config, syn::ItemStruct)) -> Result<Self> {
        Self::ensure_has_fields(&item_struct)?;
        Self::ensure_no_generics(&item_struct)?;
        Self::extract_attributes(&item_struct.attrs, config)?;
        Self::analyse_config_for_fields(&item_struct, config)?;
        config.ensure_no_conflicts()?;
        Ok(Self { item_struct })
    }
}

impl BitfieldStruct {
    /// Returns an error if the input struct does not have any fields.
    fn ensure_has_fields(item_struct: &syn::ItemStruct) -> Result<()> {
        if let unit @ syn::Fields::Unit = &item_struct.fields {
            return Err(format_err_spanned!(
                unit,
                "encountered invalid bitfield struct without fields"
            ));
        }
        Ok(())
    }

    /// Returns an error if the input struct is generic.
    fn ensure_no_generics(item_struct: &syn::ItemStruct) -> Result<()> {
        if !item_struct.generics.params.is_empty() {
            return Err(format_err_spanned!(
                item_struct,
                "encountered invalid generic bitfield struct"
            ));
        }
        Ok(())
    }

    /// Extracts the `#[repr(uN)]` annotations from the given `#[bitfield]` struct.
    fn extract_repr_attribute(attr: &syn::Attribute, config: &mut Config) -> Result<()> {
        let meta_list = attr.meta.require_list()?.clone();
        let mut retained_reprs = TokenStream::new();

        for nested_meta in meta_list.tokens {
            let meta_span = nested_meta.span();
            match nested_meta {
                TokenTree::Ident(path) => {
                    let ident = path.to_string();

                    let repr_kind = match ident.as_str() {
                        "u8" => Some(ReprKind::U8),
                        "u16" => Some(ReprKind::U16),
                        "u32" => Some(ReprKind::U32),
                        "u64" => Some(ReprKind::U64),
                        "u128" => Some(ReprKind::U128),
                        _ => {
                            // If other repr such as `transparent` or `C` have been found we
                            // are going to re-expand them into a new `#[repr(..)]` that is
                            // ignored by the rest of this macro.
                            retained_reprs.append(TokenTree::Ident(path));
                            None
                        }
                    };

                    if let Some(repr_kind) = repr_kind {
                        config.repr(repr_kind, meta_span)?;
                    }
                }
                unknown => retained_reprs.append(unknown),
            }
        }
        if !retained_reprs.is_empty() {
            // We only push back another re-generated `#[repr(..)]` if its contents
            // contained some non-bitfield representations and thus is not empty.
            config.push_retained_attribute(syn::Attribute {
                pound_token: attr.pound_token,
                style: attr.style,
                bracket_token: attr.bracket_token,
                meta: syn::Meta::List(syn::MetaList {
                    path: meta_list.path,
                    delimiter: meta_list.delimiter,
                    tokens: retained_reprs,
                }),
            });
        }
        Ok(())
    }

    /// Extracts the `#[derive(Debug)]` annotations from the given `#[bitfield]` struct.
    fn extract_derive_debug_attribute(
        attr: &syn::Attribute,
        config: &mut Config,
    ) -> Result<()> {
        let meta_list = attr.meta.require_list()?.clone();
        let mut retained_derives = TokenStream::new();

        let parser = Punctuated::<TokenTree, Token![,]>::parse_terminated;
        let list = parser.parse2(meta_list.tokens)?;

        for pair in list.into_pairs() {
            let (value, sep) = pair.into_tuple();
            let span = value.span();
            match value {
                TokenTree::Ident(path) => {
                    match path.to_string().as_str() {
                        "Debug" => {
                            config.derive_debug(span)?;
                        }
                        "BitfieldSpecifier" => {
                            config.derive_specifier(span)?;
                        }
                        _ => {
                            // Other derives are going to be re-expanded them into a new
                            // `#[derive(..)]` that is ignored by the rest of this macro.
                            retained_derives.append(TokenTree::Ident(path));
                            if let Some(sep) = sep {
                                retained_derives.extend(sep.into_token_stream())
                            }
                        }
                    };
                }
                unknown => {
                    retained_derives.append(unknown);
                    if let Some(sep) = sep {
                        retained_derives.extend(sep.into_token_stream())
                    }
                }
            }
        }
        if !retained_derives.is_empty() {
            // We only push back another re-generated `#[derive(..)]` if its contents
            // contain some remaining derives and thus is not empty.

            config.push_retained_attribute(syn::Attribute {
                pound_token: attr.pound_token,
                style: attr.style,
                bracket_token: attr.bracket_token,
                meta: syn::Meta::List(syn::MetaList {
                    path: meta_list.path,
                    delimiter: meta_list.delimiter,
                    tokens: retained_derives,
                }),
            });
        }
        Ok(())
    }

    /// Analyses and extracts the `#[repr(uN)]` or other annotations from the given struct.
    fn extract_attributes(
        attributes: &[syn::Attribute],
        config: &mut Config,
    ) -> Result<()> {
        for attr in attributes {
            if attr.path().is_ident("repr") {
                Self::extract_repr_attribute(attr, config)?;
            } else if attr.path().is_ident("derive") {
                Self::extract_derive_debug_attribute(attr, config)?;
            } else {
                config.push_retained_attribute(attr.clone());
            }
        }
        Ok(())
    }

    /// Analyses and extracts the configuration for all bitfield fields.
    fn analyse_config_for_fields(
        item_struct: &syn::ItemStruct,
        config: &mut Config,
    ) -> Result<()> {
        for (index, field) in Self::fields(item_struct) {
            let span = field.span();
            let field_config = Self::extract_field_config(field)?;
            config.field_config(index, span, field_config)?;
        }
        Ok(())
    }

    /// Extracts the `#[bits = N]` and `#[skip(..)]` attributes for a given field.
    fn extract_field_config(field: &syn::Field) -> Result<FieldConfig> {
        let mut config = FieldConfig::default();
        for attr in &field.attrs {
            if attr.path().is_ident("bits") {
                let name_value = attr.meta.require_name_value()?.clone();
                let span = name_value.span();
                match name_value.value {
                    Expr::Lit(ExprLit {
                        lit: Lit::Int(lit_int),
                        ..
                    }) => {
                        config.bits(lit_int.base10_parse::<usize>()?, span)?;
                    }
                    _ => {
                        return Err(format_err!(
                            span,
                            "encountered invalid value type for #[bits = N]"
                        ))
                    }
                }
            } else if attr.path().is_ident("skip") {
                let meta = attr.meta.clone();
                let span = meta.span();
                match meta {
                    syn::Meta::Path(path) => {
                        assert!(path.is_ident("skip"));
                        config.skip(SkipWhich::All, span)?;
                    }
                    syn::Meta::List(meta_list) => {
                        let mut which = HashMap::new();
                        let parser = Punctuated::<TokenTree, Token![,]>::parse_terminated;
                        let parts = parser.parse2(meta_list.tokens)?;
                        for nested_meta in parts {
                            match nested_meta {
                                TokenTree::Ident(path) => {

                                    match path.to_string().as_str() {
                                        "getters" => {
                                            if let Some(previous) = which.insert(SkipWhich::Getters, span)
                                            {
                                                return Err(format_err!(
                                                    span,
                                                    "encountered duplicate #[skip(getters)]"
                                                )
                                                .into_combine(format_err!(
                                                    previous,
                                                    "previous found here"
                                                )))
                                            }
                                        }
                                        "setters" => {
                                            if let Some(previous) =
                                                which.insert(SkipWhich::Setters, span)
                                            {
                                                return Err(format_err!(
                                                    span,
                                                    "encountered duplicate #[skip(setters)]"
                                                )
                                                .into_combine(format_err!(
                                                    previous,
                                                    "previous found here"
                                                )))
                                            }
                                        },
                                        other => {

                                            return Err(format_err!(
                                                path.span(),
                                                "encountered unknown or unsupported #[skip(..)] specifier `{other}`"
                                            ))
                                        }
                                    }
                                }
                                _ => return Err(format_err!(span, "encountered invalid #[skip] field attribute argument"))
                            }
                        }
                        if which.is_empty()
                            || which.contains_key(&SkipWhich::Getters)
                                && which.contains_key(&SkipWhich::Setters)
                        {
                            config.skip(SkipWhich::All, span)?;
                        } else if which.contains_key(&SkipWhich::Getters) {
                            config.skip(SkipWhich::Getters, span)?;
                        } else if which.contains_key(&SkipWhich::Setters) {
                            config.skip(SkipWhich::Setters, span)?;
                        }
                    }
                    _ => {
                        return Err(format_err!(
                            span,
                            "encountered invalid format for #[skip] field attribute"
                        ))
                    }
                }
            } else {
                config.retain_attr(attr.clone());
            }
        }
        Ok(config)
    }
}
