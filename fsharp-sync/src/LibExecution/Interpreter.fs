module LibExecution.Interpreter

// fsharplint:disable FL0039

module FnDesc =
    type T =
        { owner: string
          package: string
          module_: string
          function_: string
          version: int }

    let fnDesc (owner: string) (package: string) (module_: string) (function_: string) (version: int): T =
        { owner = owner
          package = package
          module_ = module_
          function_ = function_
          version = version }


    let stdFnDesc (module_: string) (function_: string) (version: int): T =
        fnDesc "dark" "stdlib" module_ function_ version




type Expr =
    | EInt of int
    | EString of string
    | ELet of string * Expr * Expr
    | EVariable of string
    | EFnCall of FnDesc.T * List<Expr>

type Dval =
    | DInt of int
    | DString of string
    | DError of string
    | DList of List<Dval>

module Symtable =
    type T = Map<string, Dval>
    let empty: T = Map []

    let get (st: T) (name: string): Dval =
        st.TryFind(name)
        |> Option.defaultValue (DError "Unable to find variable")

module StdLib =
    type T = { f: List<Dval> -> Dval }

    let functions (): Map<FnDesc.T, T> =
        Map
            [ (FnDesc.stdFnDesc "Int" "range" 0),
              { f =
                    (function
                    | [ DInt i; DInt j ] -> List.map DInt [ i .. j ] |> DList
                    | _ -> DError "wrong args") } ]

module Environment =
    type T = { functions: Map<FnDesc.T, StdLib.T> }
    let defaultEnv: T = { functions = StdLib.functions () }


let sfn (module_: string) (function_: string) (version: int) (args: List<Expr>): Expr =
    EFnCall(FnDesc.fnDesc "dark" "stdlib" module_ function_ version, args)



let program =
    ELet("x", sfn "Int" "range" 0 [ EInt 4; EInt 100 ], EVariable "x")



let rec eval (env: Environment.T) (st: Symtable.T) (e: Expr): Dval =
    match e with
    | EInt i -> DInt i
    | EString s -> DString s
    | ELet (lhs, rhs, body) ->
        let st = st.Add(lhs, (eval env st rhs))
        eval env st body
    | EFnCall (desc, args) ->
        match env.functions.TryFind desc with
        | Some fn -> fn.f (List.map (eval env st) args)
        | None -> DError "Function not found"
    | EVariable (name) -> Symtable.get st name

let run (e: Expr): Dval =
    eval Environment.defaultEnv Symtable.empty e

let runString (e: Expr): string = e |> run |> (fun x -> x.ToString())
