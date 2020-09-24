use crate::{
  dval,
  dval::{Dval_::*, *},
  errors::Error::*,
  expr::Expr,
  runtime::*,
};
use im_rc as im;
use itertools::Itertools;
use macros::stdlibfn;
use std::sync::Arc;
use chttp::ResponseExt;

pub struct ExecState {
  pub caller: Caller,
}

pub fn run(state: &ExecState, body: Expr) -> Dval {
  let environment = Environment { functions: stdlib(), };

  let st = im::HashMap::new();

  eval(state, body, st, &environment)
}

pub fn run_string(state: &ExecState, body: Expr) -> String {
  match &*run(state, body) {
    dval::Dval_::DSpecial(dval::Special::Error(_, err)) => {
      format!("Error: {}", err)
    }
    result => format!("{:?}", result),
  }
}

pub fn run_json(state: &ExecState, body: Expr) -> serde_json::Value {
  (run(state, body)).to_json()
}

fn stdlib() -> StdlibDef {
  #[stdlibfn]
  fn int__toString__0(a: Int) {
    dstr(&*format!("{}", *a))
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
    dint(a % b)
  }

  #[stdlibfn]
  fn hTTPClient__get__0(url: Str) {
    // Can't use async within another async block
    // let result = async {
    //   let client = hyper::Client::new();
    //   let uri = "http://localhost:1025/delay/1".parse().unwrap();
    //   let resp = client.get(uri).await.unwrap();
    //   let body_bytes = hyper::body::to_bytes(resp.into_body()).await.unwrap();
    //   let str = String::from_utf9(body_bytes.to_vec()).unwrap();
    //
    //   println!("Response: {}", str);
    //   dstr(&str)
    // };
    // tokio::runtime::Runtime::new().unwrap().block_on(result)
    let mut response = chttp::get("http://localhost:1025/delay/1").unwrap();
    let text = response.text().unwrap();
    let json : serde_json::Value = serde_json::from_str(&text).unwrap();
    let result = json["data"].as_str().unwrap();
    dstr(result)
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
                   l_symtable.update(l_vars[0].clone(), dv.clone());
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

fn eval(state: &ExecState,
        expr: Expr,
        symtable: SymTable,
        env: &Environment)
        -> Dval {
  use crate::{dval::*, expr::Expr_::*};
  match &*expr {
    IntLiteral { id: _, val } => dint(val.clone()),
    StringLiteral { id: _, val } => dstr(val),
    Blank { id } => {
      dincomplete(&Caller::Code(state.caller.to_tlid(), *id))
    }
    Let { id: _,
          lhs,
          rhs,
          body, } => {
      let rhs = eval(state, rhs.clone(), symtable.clone(), env);
      let new_symtable = symtable.update(lhs.clone(), rhs);
      eval(state, body.clone(), new_symtable, env)
    }
    Variable { id: _, name } => {
      symtable.get(name).expect("variable does not exist").clone()
    }
    Lambda { id: _,
             params,
             body, } => {
      Arc::new(DLambda(symtable, params.clone(), body.clone()))
    }
    If { id,
         cond,
         then_body,
         else_body, } => {
      let result = eval(state, cond.clone(), symtable.clone(), env);
      match *result {
        DBool(true) => {
          eval(state, then_body.clone(), symtable.clone(), env)
        }
        DBool(false) => eval(state, else_body.clone(), symtable, env),
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
            eval(state, lhs.clone(), symtable.clone(), env.clone());
          let rhs =
            eval(state, rhs.clone(), symtable.clone(), env.clone());
          let state = ExecState { caller:
                                    Caller::Code(state.caller
                                                      .to_tlid(),
                                                 *id),
                                  ..*state };

          (v.f)(&state, vec![lhs, rhs])
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
                  eval(&state, arg.clone(), symtable.clone(), env)
                })
                .collect();
          let state = ExecState { caller:
                                    Caller::Code(state.caller
                                                      .to_tlid(),
                                                 *id),
                                  ..*state };

          (v.f)(&state, args)
        }
        Option::None => {
          derror(&state.caller, MissingFunction(name.clone()))
        }
      }
    }
  }
}
