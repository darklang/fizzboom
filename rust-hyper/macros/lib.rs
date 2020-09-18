#![feature(box_patterns)]

use lazy_static::lazy_static;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use punctuated::Punctuated as Punc;
use quote::{self, ToTokens};
use regex::Regex;
use std::iter::FromIterator;
use syn::*;

fn ident(name: &str) -> Ident {
  Ident::new(name, Span::call_site())
}

fn ident_pat(name: &str) -> Pat {
  Pat::from(PatIdent { attrs:      Vec::new(),
                       by_ref:     None,
                       mutability: None,
                       subpat:     None,
                       ident:      ident(name), })
}

fn get_arguments(ifn: ItemFn) -> Vec<(String, Type)> {
  let x =
    ifn.sig
       .inputs
       .iter()
       .map(|fn_arg| match fn_arg {
         FnArg::Receiver(_) => {
           panic!("Got \"self\", expected a simple argument name")
         }
         FnArg::Typed(PatType { pat: box Pat::Ident(pat),
                                box ty,
                                .. }) => {
           (pat.ident.to_string(), ty.clone())
         }

         _ => panic!("invalid type"),
       })
       .collect();
  x
}

fn path(scopes: Vec<&str>, name: &str) -> Path {
  Path { leading_colon: None,
         segments:      {
           let mut r = Punc::new();
           for s in scopes {
             r.push(PathSegment { ident:     ident(s),
                                  arguments: PathArguments::None, })
           }
           r.push(PathSegment { ident:     ident(name),
                                arguments: PathArguments::None, });
           r
         }, }
}

fn variant(scopes: Vec<&str>,
           variant_name: &str,
           args: Vec<&str>)
           -> Pat {
  let elems: Punc<Pat, token::Comma> =
    Punc::from_iter(args.iter().map(|arg| ident_pat(arg)));
  Pat::TupleStruct (PatTupleStruct { attrs: Vec::new(),
                     path:  path(scopes, variant_name),
                     pat:
                       PatTuple { attrs: Vec::new(),
                                  paren_token:
                                    token::Paren { span:
                                                     Span::call_site(), },
                                  elems }, })
}

fn argument_pattern(scopes: Vec<&str>, name: &str, ty: &Type) -> Pat {
  let type_name = match ty {
    Type::Path(TypePath { path, .. }) => {
      Path::get_ident(path).unwrap()
    }
    _ => panic!("Not a path"),
  };
  match type_name.to_string().as_ref() {
    "Int" => variant(scopes, "DInt", vec![name]),
    "List" => variant(scopes, "DList", vec![name]),
    "Lambda" => {
      variant(scopes,
              "DLambda",
              vec![&format!("{}_symtable", name).to_string(),
                   &format!("{}_vars", name).to_string(),
                   &format!("{}_body", name).to_string()])
    }
    ty => panic!("type not recognized: {}", ty),
  }
}

fn get_argument_patterns(ifn: ItemFn) -> Punc<Pat, token::Comma> {
  // For (start : int, l : lambda::<[A], B>)
  // Returns DInt(start), DLambda(l_vars, l_body)a
  Punc::from_iter(get_arguments(ifn).iter()
                                    .map(|(name, ty)| {
                                      argument_pattern(vec!["dval",
                                                            "Dval_"],
                                                       name,
                                                       ty)
                                    })
                                    .into_iter())
}

fn get_body(ifn: ItemFn) -> Box<Block> {
  ifn.block
}
//
// fn get_types(_ifn: ItemFn)
//              -> Punc<FnArg, token::Comma> {
//   Punc::new()
// }
//
fn get_fn_name(ifn: ItemFn) -> Ident {
  ifn.sig.ident
}

struct FunctionDesc {
  owner:    String,
  package:  String,
  module:   String,
  function: String,
  version:  u32,
}

#[derive(Debug)]
struct FunctionDescError {}

impl std::str::FromStr for FunctionDesc {
  type Err = FunctionDescError;

  fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
    lazy_static! {
      static ref STDLIB_RE: Regex =
        Regex::new(r"(^[a-zA-Z0-9]+)__([a-zA-Z0-9]+)__([0-9]+)$").unwrap();
      static ref RE : Regex =
        Regex::new(r"(^[a-zA-Z0-9]+)__([a-zA-Z0-9]+)__([a-zA-Z0-9]+)__([a-zA-Z0-9]+)__([0-9]+)$").unwrap();
    }
    fn cgroup(c: &regex::Captures, i: usize) -> String {
      c.get(i)
       .expect(&*format!("expected capture group {}", i,))
       .as_str()
       .to_string()
    }
    fn capitalize(s: &str) -> String {
      let mut c = s.chars();
      match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
      }
    }
    fn rename(s: &str) -> String {
      match s {
        "eq" => "==".to_string(),
        "mod" => "%".to_string(),
        other => other.to_string(),
      }
    }
    match RE.captures(s) {
      Some(c) if c.len() == 6 => Ok(FunctionDesc { owner: cgroup(&c, 1),

                                   package:  cgroup(&c, 2),
                                   module:   capitalize(&cgroup(&c, 3)),
                                   function: rename(&cgroup(&c, 4)),
                                   version:
                                     cgroup(&c, 5).parse::<u32>()
                                                  .unwrap(), }),
      None => match STDLIB_RE.captures(s) {
        Some(c) if c.len() == 4 => Ok(FunctionDesc { owner:   "dark".to_string(),
                                     package: "stdlib".to_string(),

                                     module:   capitalize(&cgroup(&c, 1)),
                                     function: rename(&cgroup(&c, 2)),
                                     version:
                                       cgroup(&c, 3).parse::<u32>()
                                                    .expect("expected version to be int"), }),

        _ => panic!("fn name doesn't match expected pattern: {}", s),
      },

        _ => panic!("fn name doesn't match expected pattern: {}", s),
    }
  }
}

fn get_fn_name_parts(name: &str) -> FunctionDesc {
  str::parse::<FunctionDesc>(name).unwrap()
}

#[proc_macro_attribute]
pub fn stdlibfn(_attr: TokenStream,
                item: TokenStream)
                -> TokenStream {
  let input = syn::parse_macro_input!(item as syn::ItemFn);
  let body = get_body(input.clone());
  let argument_patterns = get_argument_patterns(input.clone());
  // let _types = get_types(input.clone());
  let fn_name = get_fn_name(input.clone());
  let FunctionDesc { owner,
                     package,
                     module,
                     function,
                     version, } =
    get_fn_name_parts(&fn_name.to_string());
  // TODO: add types
  let fn_stmt: syn::Stmt = parse_quote! {


    #[allow(non_snake_case)]
    fn #fn_name() -> (FunctionDesc_, StdlibFunction) {
        let fn_name = FunctionDesc_::FunctionDesc(
            #owner.to_string(),
            #package.to_string(),
            #module.to_string(),
            #function.to_string(),
            #version,
        );
        let fn_name2 = fn_name.clone();
          (fn_name,
         StdlibFunction {
           f:
             {
               Arc::new(
                 move |state : &ExecState, args : Vec<Dval>| { {
                   match args.iter().map(|v| &(**v)).collect::<Vec<_>>().as_slice() {
                     [ #argument_patterns ] => #body,
                     _ => {
                       for arg in args.clone() {
                         if (arg).is_special() { return arg.clone ()}
                       }
                       Arc::new(Dval_::DSpecial((Special::Error(state.caller, IncorrectArguments(fn_name2.clone(), args)))))
                     }}}})},
                    })}

  };
  TokenStream::from(fn_stmt.into_token_stream())
}
