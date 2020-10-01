use criterion::{black_box, criterion_main, criterion_group, Criterion};

use execution_engine::{eval::{run_json, ExecState}, expr::*};

fn fizzbuzz<'a>() -> Expr<'a> {
    elet("range",
        esfn("Int", "range", 0, [eint(1), eint(100),].into()),
        esfn("List",
            "map",
            0,
            [(evar("range")),
                elambda((&["i"][..]).into(),
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
                                            [evar("i")].into())))))].into()))
}
  

fn eval_fizzbuzz(c: &mut Criterion) {
    c.bench_function("fizzbuzz", |b| b.iter(|| {
        let program = black_box(fizzbuzz());
        
        let tlid = execution_engine::runtime::TLID::TLID(7);
        let state = ExecState { caller: execution_engine::runtime::Caller::Toplevel(tlid), };

        run_json(&state, &program);
    }));
}

criterion_group!(benches, eval_fizzbuzz);
criterion_main!(benches);
