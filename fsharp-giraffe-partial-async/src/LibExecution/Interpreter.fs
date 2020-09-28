module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Data
open FSharp.Data.JsonExtensions
open FSharp.Control.Tasks

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
    | EInt of bigint
    | EString of string
    | ELet of string * Expr * Expr
    | EVariable of string
    | EFnCall of FnDesc.T * List<Expr>
    | EBinOp of Expr * FnDesc.T * Expr
    | ELambda of List<string> * Expr
    | EIf of Expr * Expr * Expr

type Dval =
    | DInt of bigint
    | DString of string
    | DSpecial of Special
    | DList of List<Dval>
    | DBool of bool
    | DLambda of Symtable * List<string> * Expr

    member this.isSpecial: bool =
        match this with
        | DSpecial _ -> true
        | _ -> false

    member this.toJSON(): FSharp.Data.JsonValue =
        match this with
        | DInt i -> JsonValue.Number(decimal i)
        | DString str -> JsonValue.String(str)

        | DList l ->
            l
            |> List.map (fun dv -> dv.toJSON ())
            |> List.toArray
            |> JsonValue.Array

        | DBool b -> JsonValue.Boolean b
        | DLambda _ -> JsonValue.Null
        | DSpecial (DError (e)) -> JsonValue.Record [| "error", JsonValue.String(e.ToString()) |]

    static member toDList(list: List<Dval>): Dval =
        List.tryFind (fun (dv: Dval) -> dv.isSpecial) list
        |> Option.defaultValue (DList list)

and Symtable = Map<string, Dval>

and Param =
    { name: string
      tipe: DType
      doc: string }

(* Runtime errors can be things that happen relatively commonly (such as calling
   a function with an incorrect type), or things that aren't supposed to happen
   but technically can (such as accessing a variable which doesn't exist)
*)
and RuntimeError =
    | NotAFunction of FnDesc.T
    | CondWithNonBool of Dval
    | FnCalledWithWrongTypes of FnDesc.T * List<Dval> * List<Param>
    | FnCalledWhenNotSync of FnDesc.T * List<Dval> * List<Param>
    | UndefinedVariable of string


and Special = DError of RuntimeError

and DType =
    | TString
    | TInt
    | TBool
    | TList of DType
    (* A named variable, eg `a` in `List<a>` *)
    | TVariable of string
    | TFn of List<DType> * DType

let err (e: RuntimeError) = DSpecial(DError(e))

module Symtable =
    type T = Symtable
    let empty: T = Map []

    let get (st: T) (name: string): Dval =
        st.TryFind(name)
        |> Option.defaultValue (err (UndefinedVariable name))


module Environment =
    type RetVal = { tipe: DType; doc: string }

    type BuiltInFn =
        | Sync of SyncFn
        | Async of AsyncFn

    and SyncFn =
        { name: FnDesc.T
          parameters: List<Param>
          returnVal: RetVal
          fn: (T * List<Dval>) -> Result<Dval, unit> }

    and AsyncFn =
        { name: FnDesc.T
          parameters: List<Param>
          returnVal: RetVal
          fn: (T * List<Dval>) -> Task<Result<Dval, unit>> }

    and T = { functions: Map<FnDesc.T, BuiltInFn> }

    let envWith (functions: Map<FnDesc.T, BuiltInFn>): T = { functions = functions }

let param (name: string) (tipe: DType) (doc: string): Param = { name = name; tipe = tipe; doc = doc }
let retVal (tipe: DType) (doc: string): Environment.RetVal = { tipe = tipe; doc = doc }


let sfn (module_: string) (function_: string) (version: int) (args: List<Expr>): Expr =
    EFnCall(FnDesc.fnDesc "dark" "stdlib" module_ function_ version, args)

let binOp (arg1: Expr) (module_: string) (function_: string) (version: int) (arg2: Expr): Expr =
    EBinOp(arg1, FnDesc.fnDesc "dark" "stdlib" module_ function_ version, arg2)

let fizzbuzz: Expr =
    ELet
        ("range",
         (sfn "Int" "range" 0 [ EInt(bigint 1); EInt(bigint 100) ]),
         (sfn
             "List"
              "map"
              0
              [ EVariable "range"
                (ELambda
                    ([ "i" ],
                     EIf
                         ((binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 15))) "Int" "==" 0 (EInt(bigint 0))),
                          EString "fizzbuzz",
                          EIf
                              (binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 5))) "Int" "==" 0 (EInt(bigint 0)),
                               EString "buzz",
                               EIf
                                   (binOp
                                       (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 3)))
                                        "Int"
                                        "=="
                                        0
                                        (EInt(bigint 0)),
                                    EString "fizz",
                                    sfn "Int" "toString" 0 [ EVariable "i" ]))))) ]))

let fizzboom: Expr =
    ELet
        ("range",
         (sfn "Int" "range" 0 [ EInt(bigint 1); EInt(bigint 100) ]),
         (sfn
             "List"
              "map"
              0
              [ EVariable "range"
                (ELambda
                    ([ "i" ],
                     EIf
                         ((binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 15))) "Int" "==" 0 (EInt(bigint 0))),
                          (sfn "HttpClient" "get" 0 [ EString "http://localhost:1025/delay/1" ]),
                          EIf
                              (binOp (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 5))) "Int" "==" 0 (EInt(bigint 0)),
                               EString "buzz",
                               EIf
                                   (binOp
                                       (binOp (EVariable "i") "Int" "%" 0 (EInt(bigint 3)))
                                        "Int"
                                        "=="
                                        0
                                        (EInt(bigint 0)),
                                    EString "fizz",
                                    sfn "Int" "toString" 0 [ EVariable "i" ]))))) ]))

let map_s (list: List<'a>) (f: ('a -> Task<'b>)): Task<List<'b>> =
    task {
        let! result =
            match list with
            | [] -> task { return [] }
            | head :: tail ->
                task {
                    let firstComp =
                        task {
                            let! result = f head
                            return ([], result)
                        }

                    let! ((accum, lastcomp): (List<'b> * 'b)) =
                        List.fold (fun (prevcomp: Task<List<'b> * 'b>) (arg: 'a) ->
                            task {
                                // Ensure the previous computation is done first
                                let! ((accum, prev): (List<'b> * 'b)) = prevcomp
                                let accum = prev :: accum

                                let! result = f arg

                                return (accum, result)
                            }) firstComp tail

                    return List.rev (lastcomp :: accum)
                }

        return (result |> Seq.toList)
    }


let rec evalAsync (env: Environment.T) (st: Symtable.T) (e: Expr): Task<Dval> =
    let tryFindFn desc = env.functions.TryFind(desc)

    match e with
    | EInt i -> task { return DInt i }
    | EString s -> task { return DString s }
    | ELet (lhs, rhs, body) ->
        task {
            let! rhs = evalAsync env st rhs
            let st = st.Add(lhs, rhs)
            return! (evalAsync env st body)
        }
    | EFnCall (desc, args) ->
        (match tryFindFn desc with
         | Some fn ->
             task {
                 let! args = map_s args (evalAsync env st)
                 return! call_fn_async env fn (Seq.toList args)
             }
         | None -> task { return (err (NotAFunction desc)) })
    | EBinOp (arg1, desc, arg2) ->
        (match tryFindFn desc with
         | Some fn ->
             task {
                 let! arg1 = evalAsync env st arg1
                 let! arg2 = evalAsync env st arg2
                 return! call_fn_async env fn [ arg1; arg2 ]
             }
         | None -> task { return (err (NotAFunction desc)) })
    | ELambda (vars, expr) -> task { return DLambda(st, vars, expr) }
    | EVariable (name) -> task { return Symtable.get st name }
    | EIf (cond, thenbody, elsebody) ->
        task {
            let! cond = evalAsync env st cond

            match cond with
            | DBool (true) -> return! evalAsync env st thenbody
            | DBool (false) -> return! evalAsync env st elsebody
            | _ -> return err (CondWithNonBool cond)
        }

and call_fn_async (env: Environment.T) (fn: Environment.BuiltInFn) (args: List<Dval>): Task<Dval> =
    task {
        match List.tryFind (fun (dv: Dval) -> dv.isSpecial) args with
        | Some special -> return special
        | None ->
            match fn with
            | Environment.BuiltInFn.Async fn ->
                match! fn.fn (env, args) with
                | Ok result -> return result
                | Error () -> return err (FnCalledWithWrongTypes(fn.name, args, fn.parameters))
            | Environment.BuiltInFn.Sync fn ->
                match fn.fn (env, args) with
                | Ok result -> return result
                | Error () -> return err (FnCalledWithWrongTypes(fn.name, args, fn.parameters))
    }

module StdLib =
    let functions (): Map<FnDesc.T, Environment.BuiltInFn> =
        let fns: List<Environment.BuiltInFn> =
            [ Environment.Sync
                { name = (FnDesc.stdFnDesc "Int" "range" 0)
                  parameters =
                      [ param "list" (TList(TVariable("a"))) "The list to be operated on"
                        param "fn" (TFn([ TVariable("a") ], TVariable("b"))) "Function to be called on each member" ]
                  returnVal = retVal (TList(TInt)) "List of ints between lowerBound and upperBound"
                  fn =
                      (function
                      | _, [ DInt lower; DInt upper ] -> List.map DInt [ lower .. upper ] |> DList |> Ok
                      | _ -> Error()) }
              Environment.Async
                  { name = (FnDesc.stdFnDesc "List" "map" 0)
                    parameters =
                        [ param "list" (TList(TVariable("a"))) "The list to be operated on"
                          param "fn" (TFn([ TVariable("a") ], TVariable("b"))) "Function to be called on each member" ]
                    returnVal =
                        (retVal
                            (TList(TVariable("b")))
                             "A list created by the elements of `list` with `fn` called on each of them in order")
                    fn =
                        (function
                        | env, [ DList l; DLambda (st, [ var ], body) ] ->
                            task {
                                let! result =
                                    map_s l (fun dv ->
                                        let st = st.Add(var, dv)
                                        evalAsync env st body)

                                return (result |> Dval.toDList |> Ok)
                            }

                        | _ -> task { return Error() }) }
              Environment.Sync
                  { name = (FnDesc.stdFnDesc "Int" "%" 0)
                    parameters =
                        [ param "a" TInt "Numerator"
                          param "b" TInt "Denominator" ]
                    returnVal = (retVal TInt "Returns the modulus of a / b")
                    fn =
                        (function
                        | env, [ DInt a; DInt b ] ->
                            try
                                Ok(DInt(a % b))
                            with _ -> Ok(DInt(bigint 0))
                        | _ -> Error()) }
              Environment.Sync
                  { name = (FnDesc.stdFnDesc "Int" "==" 0)
                    parameters =
                        [ param "a" TInt "a"
                          param "b" TInt "b" ]
                    returnVal =
                        (retVal
                            TBool
                             "True if structurally equal (they do not have to be the same piece of memory, two dicts or lists or strings with the same value will be equal), false otherwise")
                    fn =
                        (function
                        | env, [ DInt a; DInt b ] -> Ok(DBool(a = b))
                        | _ -> Error()) }
              Environment.Sync
                  { name = (FnDesc.stdFnDesc "Int" "toString" 0)
                    parameters = [ param "a" TInt "value" ]
                    returnVal = (retVal TString "Stringified version of a")
                    fn =
                        (function
                        | env, [ DInt a ] -> Ok(DString(a.ToString()))
                        | _ -> Error()) }
              Environment.Async
                  { name = (FnDesc.stdFnDesc "HttpClient" "get" 0)
                    parameters = [ param "url" TString "URL to fetch" ]
                    returnVal = (retVal TString "Body of response")
                    fn =
                        (function
                        | env, [ DString url ] ->
                            try
                                task {
                                    let! response = Http.AsyncRequestString(url)
                                    let info = JsonValue.Parse(response)
                                    return Ok(DString(info?data.AsString()))
                                }
                            with e ->
                                printfn "error in HttpClient::get: %s" (e.ToString())
                                task { return Error() }
                        | _ -> task { return Error() }) } ]

        fns
        |> List.map (fun fn ->
            match fn with
            | Environment.Async f -> (f.name), fn
            | Environment.Sync f -> (f.name), fn)
        |> Map


let env =
    Environment.envWith (StdLib.functions ())

let runAsync (e: Expr): Task<Dval> = evalAsync env Symtable.empty e

let runJSONAsync (e: Expr): Task<string> =
    task {
        let! result = runAsync e
        let json = result.toJSON ()
        return json.ToString()
    }
