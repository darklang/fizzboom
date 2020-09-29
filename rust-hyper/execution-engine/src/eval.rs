use crate::{
  dval,
  dval::{Dval::*, *},
  errors::Error::*,
  expr::Expr,
  runtime::*,
};
use im_rc as im;
use itertools::Itertools;
use macros::stdlibfn;
use std::borrow::Cow;

pub struct ExecState {
  pub caller: Caller,
}

pub fn run<'a, 'b>(state: &'b ExecState, body: &'a  Expr<'a>) -> Dval<'a> {
  let environment = Environment { functions: stdlib(), };

  let st = im::HashMap::new();

  eval(state, body, st, &environment)
}

pub fn run_string<'a>(state: &'a ExecState, body: &'a Expr<'a>) -> String {
  match run(state, body) {
    dval::Dval::DSpecial(box dval::Special::Error(_, err)) => {
      format!("Error: {}", err)
    }
    result => format!("{:?}", result),
  }
}

pub fn run_json<'a, 'b>(state: &'b ExecState, body: &'a Expr<'a>) -> String {
  serde_json::to_string(&run(state, body)).unwrap()
}

fn stdlib() -> StdlibDef<'static> {
  #[stdlibfn]
  fn int__toString__0(a: Int) {
    dstr(Cow::Owned(format!("{}", *a)))
  }

  #[stdlibfn]
  fn int__range__0(start: Int, end: Int) -> Dval {
    let mut result = im::Vector::new();
    let mut i = start.clone();
    while &i <= end {
      result.push_back(dint(i.clone()));
      i += 1;
    }
    dlist(result)
  }

  #[stdlibfn]
  fn int__random32__0() {
    dint(ramp::Int::from(rand::random::<i32>()))
  }

  #[stdlibfn]
  fn int__random64__0() {
    dint(ramp::Int::from(rand::random::<i64>()))
  }

  #[stdlibfn]
  fn int__eq__0(a: Int, b: Int) {
    dbool(a == b)
  }

  #[stdlibfn]
  fn int__mod__0(a: Int, b: Int) {
    if b.eq(&0) {
      derror(&state.caller, crate::errors::Error::IncorrectArguments(fn_name, vec![]))
    } else {
      dint(a % b)
    }
  }

  #[stdlibfn]
  fn hTTPClient__get__0(url: Str) {
    lazy_static::lazy_static! {
      static ref CLIENT: reqwest::blocking::Client = reqwest::blocking::Client::new();
    }

    let response = CLIENT.get("http://localhost:1025/delay/1").send().unwrap();
    let text = response.text().unwrap();
    let json : serde_json::Value = serde_json::from_str(&text).unwrap();
    let result = json["data"].as_str().unwrap().to_string();
    dstr(Cow::Owned(result))
  }

  #[stdlibfn]
  fn list__map__0(members: List, l: Lambda) {
    {
      let new_list =
        members.iter()
               .map(|dv| {
                 let environment =
                   Environment { functions: stdlib(), };
                 let st =
                   l_symtable.update(l_vars[0], dv.clone());
                 let result = eval(state,
                                   l_body.clone(),
                                   st.clone(),
                                   &environment);
                 if result.is_special() {
                   return Err(result)
                 }
                 Ok(result)
               })
               .fold_results(im::Vector::new(), |mut accum, item| {
                 accum.push_back(item);
                 accum
               });
      match new_list {
        Ok(r) => dlist(r),
        Err(special) => special,
      }
    }
  }

  let fns = vec![int__random32__0(),
                 int__random64__0(),
                 int__range__0(),
                 list__map__0(),
                 hTTPClient__get__0(),
                 int__toString__0(),
                 int__eq__0(),
                 int__mod__0(),];

  fns.into_iter().collect()
}

fn eval<'a, 'b>(state: &'b ExecState,
        expr: &'a Expr<'a>,
        symtable: SymTable<'a>,
        env: &'b Environment)
        -> Dval<'a> {
  use crate::{dval::*, expr::Expr::*};
  match expr {
    IntLiteral { id: _, val } => dint(val.clone()),
    StringLiteral { id: _, val } => dstr(Cow::Borrowed(val)),
    Blank { id } => {
      dincomplete(&Caller::Code(state.caller.to_tlid(), *id))
    }
    Let { id: _,
          lhs,
          rhs,
          body, } => {
      let rhs = eval(state, rhs, symtable.clone(), env);
      let new_symtable = symtable.update(lhs, rhs);
      eval(state, body, new_symtable, env)
    }
    Variable { id: _, name } => {
      symtable.get(*name).expect("variable does not exist").clone()
    }
    Lambda { id: _,
             params,
             body, } => {
      DLambda(symtable, params, body)
    }
    If { id,
         cond,
         then_body,
         else_body, } => {
      let result = eval(state, cond, symtable.clone(), env);
      match result {
        DBool(true) => {
          eval(state, then_body, symtable, env)
        }
        DBool(false) => eval(state, else_body, symtable, env),
        _ => dcode_error(&state.caller,
                         *id,
                         InvalidType(result, dval::DType::TBool)),
      }
    }
    BinOp { id, lhs, op, rhs } => {
      let fn_def = env.functions.get(op);

      match fn_def {
        Option::Some(v) => {
          let lhs =
            eval(state, lhs, symtable.clone(), env);
          let rhs =
            eval(state, rhs, symtable, env);
          let state = ExecState { caller:
                                    Caller::Code(state.caller
                                                      .to_tlid(),
                                                 *id),
                                  ..*state };

          (v.f)(&state, &[lhs, rhs])
        }
        Option::None => {
          derror(&state.caller, MissingFunction(op.clone()))
        }
      }
    }

    FnCall { id, name, args } => {
      let fn_def = env.functions.get(name);

      match fn_def {
        Option::Some(v) => {
          let args: Vec<Dval> =
            args.into_iter()
                .map(|arg| {
                  eval(&state, arg, symtable.clone(), env)
                })
                .collect();
          let state = ExecState { caller:
                                    Caller::Code(state.caller
                                                      .to_tlid(),
                                                 *id),
                                  ..*state };

          (v.f)(&state, &args)
        }
        Option::None => {
          derror(&state.caller, MissingFunction(name.clone()))
        }
      }
    }
  }
}
