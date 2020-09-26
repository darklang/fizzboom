#![feature(trace_macros)]
#![feature(box_syntax)]
#![feature(log_syntax)]

use hyper::{
  service::{make_service_fn, service_fn},
  Body, Request, Response, Server, Method, StatusCode
};
use std::{convert::Infallible, net::SocketAddr};

use execution_engine::{self, eval, expr::*, ivec, runtime};

fn fizzboom<'a>() -> Expr<'a> {
  elet("range",
       esfn("Int", "range", 0, ivec![eint(1), eint(100),]),
       esfn("List",
            "map",
            0,
            ivec![(evar("range")),
                  elambda(ivec!["i"],
                          eif(ebinop(ebinop(evar("i"),
                                            "Int",
                                            "%",
                                            0,
                                            eint(15)),
                                     "Int",
                                     "==",
                                     0,
                                     eint(0)),
                              esfn("HTTPClient", "get", 0, ivec![estr("http://localhost:1025/delay/1")]),
                              eif(ebinop(ebinop(evar("i"),
                                                "Int",
                                                "%",
                                                0,
                                                eint(5)),
                                         "Int",
                                         "==",
                                         0,
                                         eint(0)),
                                  estr("buzz"),
                                  eif(ebinop(ebinop(evar("i"),
                                                    "Int",
                                                    "%",
                                                    0,
                                                    eint(3)),
                                             "Int",
                                             "==",
                                             0,
                                             eint(0)),
                                      estr("fizz"),
                                      esfn("Int",
                                           "toString",
                                           0,
                                           ivec![evar("i")])))))]))
}
fn fizzbuzz<'a>() -> Expr<'a> {
  elet("range",
       esfn("Int", "range", 0, ivec![eint(1), eint(100),]),
       esfn("List",
            "map",
            0,
            ivec![(evar("range")),
                  elambda(ivec!["i"],
                          eif(ebinop(ebinop(evar("i"),
                                            "Int",
                                            "%",
                                            0,
                                            eint(15)),
                                     "Int",
                                     "==",
                                     0,
                                     eint(0)),
                              estr("fizzbuzz"),
                              eif(ebinop(ebinop(evar("i"),
                                                "Int",
                                                "%",
                                                0,
                                                eint(5)),
                                         "Int",
                                         "==",
                                         0,
                                         eint(0)),
                                  estr("buzz"),
                                  eif(ebinop(ebinop(evar("i"),
                                                    "Int",
                                                    "%",
                                                    0,
                                                    eint(3)),
                                             "Int",
                                             "==",
                                             0,
                                             eint(0)),
                                      estr("fizz"),
                                      esfn("Int",
                                           "toString",
                                           0,
                                           ivec![evar("i")])))))]))
}

async fn run_program<'a>(program: Expr<'a>)
                     -> String {
  let tlid = runtime::TLID::TLID(7);
  let state =
    eval::ExecState { caller: runtime::Caller::Toplevel(tlid), };
  
  eval::run_json(&state, program)
}

async fn handle(req: Request<Body>)
                     -> Result<Response<Body>, Infallible> {
  let mut response = Response::new(Body::empty());

  match (req.method(), req.uri().path()) {
      (&Method::GET, "/fizzbuzz") => {
        let r = fizzbuzz();
        let f = run_program(r).await;
          *response.body_mut() = Body::from(f);
      },
      (&Method::GET, "/fizzboom") => {
          *response.body_mut() = Body::from(run_program(fizzboom()).await);
      },
      _ => {
          *response.status_mut() = StatusCode::NOT_FOUND;
      },
  };
  Ok(response)
}

#[tokio::main]
async fn main() {
  let port = std::fs::read_to_string("port").unwrap()
                                            .trim()
                                            .parse::<u16>()
                                            .unwrap();
  let addr = SocketAddr::from(([127, 0, 0, 1], port));
  // A `Service` is needed for every connection, so this
  // creates one from our `hello_world` function.
  let make_svc = make_service_fn(|_conn| async {
    // service_fn converts our function into a `Service`
    Ok::<_, Infallible>(service_fn(handle))
  });

  let server = Server::bind(&addr).serve(make_svc);

  // Run this server for... forever!
  if let Err(e) = server.await {
    eprintln!("server error: {}", e);
  }
}
