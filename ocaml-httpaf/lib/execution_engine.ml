open Base
module C = Curl

module FnDesc = struct
  module T = struct
    type t =
      { owner : string
      ; package : string
      ; module_ : string
      ; function_ : string
      ; version : int }
    [@@deriving sexp, compare]
  end

  include T
  include Comparator.Make (T)

  let fnDesc
      (owner : string)
      (package : string)
      (module_ : string)
      (function_ : string)
      (version : int) : t =
    {owner; package; module_; function_; version}


  let stdFnDesc (module_ : string) (function_ : string) (version : int) : t =
    fnDesc "dark" "stdlib" module_ function_ version
end

type expr =
  | EInt of Z.t
  | EString of string
  | ELet of string * expr * expr
  | EVariable of string
  | EFnCall of FnDesc.t * expr list
  | EBinOp of expr * FnDesc.t * expr
  | ELambda of string list * expr
  | EIf of expr * expr * expr

type dval =
  | DInt of Z.t
  | DString of string
  | DSpecial of special
  | DList of dval list
  | DBool of bool
  | DLambda of symtable * string list * expr

and symtable = (string, dval, String.comparator_witness) Map.t

and param =
  { name : string
  ; tipe : typ
  ; doc : string }

(* Runtime errors can be things that happen relatively commonly (such as calling
   a function with an incorrect type), or things that aren't supposed to happen
   but technically can (such as accessing a variable which doesn't exist)
*)
and runtimeError =
  | NotAFunction of FnDesc.t
  | CondWithNonBool of dval
  | FnCalledWithWrongTypes of FnDesc.t * dval list * param list
  | UndefinedVariable of string

and special = DError of runtimeError

and typ =
  | TString
  | TInt
  | TBool
  | TList of typ
  (* A named variable, eg `a` in `List<a>` *)
  | TVariable of string
  | TFn of typ list * typ

module Dval = struct
  type t = dval

  let is_special (dv : t) : bool =
    match dv with DSpecial _ -> true | _ -> false


  let rec to_json (dv : t) : Yojson.Safe.t =
    match dv with
    | DInt i ->
        `Intlit (i |> Z.to_string)
    | DString str ->
        `String str
    | DList l ->
        `List (l |> List.map ~f:to_json)
    | DBool b ->
        `Bool b
    | DLambda _ ->
        `Null
    | DSpecial _ ->
        `Null


  let toDList (list : t list) : dval =
    List.find ~f:is_special list |> Option.value ~default:(DList list)
end

let err (e : runtimeError) = DSpecial (DError e)

module Symtable = struct
  type t = symtable

  let empty : t = Map.empty (module String)

  let get (st : t) (name : string) : dval =
    Map.find st name |> Option.value ~default:(err (UndefinedVariable name))
end

module Environment = struct
  type ret_val =
    { tipe : typ
    ; doc : string }

  and built_in_fn =
    { name : FnDesc.t
    ; parameters : param list
    ; return_val : ret_val
    ; fn : t * dval list -> (dval, unit) Result.t }

  and fn_map = (FnDesc.t, built_in_fn, FnDesc.comparator_witness) Map.t

  and t = {functions : fn_map}

  let envWith (functions : fn_map) : t = {functions}
end

let param (name : string) (tipe : typ) (doc : string) : param = {name; tipe; doc}

let ret_val (tipe : typ) (doc : string) : Environment.ret_val = {tipe; doc}

let sfn
    (module_ : string) (function_ : string) (version : int) (args : expr list) :
    expr =
  EFnCall (FnDesc.fnDesc "dark" "stdlib" module_ function_ version, args)


let binOp
    (arg1 : expr)
    (module_ : string)
    (function_ : string)
    (version : int)
    (arg2 : expr) : expr =
  EBinOp (arg1, FnDesc.fnDesc "dark" "stdlib" module_ function_ version, arg2)


let var str = EVariable str

let int (i : int) = EInt (Z.of_int i)

let str s = EString s

let fizzbuzz : expr =
  ELet
    ( "range"
    , sfn "Int" "range" 0 [int 1; int 100]
    , sfn
        "List"
        "map"
        0
        [ var "range"
        ; ELambda
            ( ["i"]
            , EIf
                ( binOp
                    (binOp (var "i") "Int" "%" 0 (int 15))
                    "Int"
                    "=="
                    0
                    (int 0)
                , str "fizzbuzz"
                , EIf
                    ( binOp
                        (binOp (var "i") "Int" "%" 0 (int 5))
                        "Int"
                        "=="
                        0
                        (int 0)
                    , EString "buzz"
                    , EIf
                        ( binOp
                            (binOp (var "i") "Int" "%" 0 (int 3))
                            "Int"
                            "=="
                            0
                            (int 0)
                        , str "fizz"
                        , sfn "Int" "toString" 0 [var "i"] ) ) ) ) ] )


let fizzboom : expr =
  ELet
    ( "range"
    , sfn "Int" "range" 0 [int 1; int 100]
    , sfn
        "List"
        "map"
        0
        [ var "range"
        ; ELambda
            ( ["i"]
            , EIf
                ( binOp
                    (binOp (var "i") "Int" "%" 0 (int 15))
                    "Int"
                    "=="
                    0
                    (int 0)
                , sfn "HttpClient" "get" 0 [str "http://localhost:1025/delay/1"]
                , EIf
                    ( binOp
                        (binOp (var "i") "Int" "%" 0 (int 5))
                        "Int"
                        "=="
                        0
                        (int 0)
                    , EString "buzz"
                    , EIf
                        ( binOp
                            (binOp (var "i") "Int" "%" 0 (int 3))
                            "Int"
                            "=="
                            0
                            (int 0)
                        , str "fizz"
                        , sfn "Int" "toString" 0 [var "i"] ) ) ) ) ] )


let rec eval (env : Environment.t) (st : Symtable.t) (e : expr) : dval =
  match e with
  | EInt i ->
      DInt i
  | EString s ->
      DString s
  | ELet (lhs, rhs, body) ->
      let rhs = eval env st rhs in
      let st = Map.set st lhs rhs in
      eval env st body
  | EFnCall (desc, args) ->
    ( match Map.find env.functions desc with
    | Some fn ->
        let args = List.map ~f:(eval env st) args in
        call_fn env fn args
    | None ->
        err (NotAFunction desc) )
  | EBinOp (arg1, desc, arg2) ->
    ( match Map.find env.functions desc with
    | Some fn ->
        let arg1 = eval env st arg1 in
        let arg2 = eval env st arg2 in
        call_fn env fn [arg1; arg2]
    | None ->
        err (NotAFunction desc) )
  | ELambda (vars, expr) ->
      DLambda (st, vars, expr)
  | EVariable name ->
      Symtable.get st name
  | EIf (cond, thenbody, elsebody) ->
      let cond = eval env st cond in

      ( match cond with
      | DBool true ->
          eval env st thenbody
      | DBool false ->
          eval env st elsebody
      | _ ->
          err (CondWithNonBool cond) )


and call_fn
    (env : Environment.t) (fn : Environment.built_in_fn) (args : dval list) :
    dval =
  match List.find ~f:Dval.is_special args with
  | Some special ->
      special
  | None ->
      let result = fn.fn (env, args) in
      ( match result with
      | Ok result ->
          result
      | Error () ->
          err (FnCalledWithWrongTypes (fn.name, args, fn.parameters)) )


module StdLib = struct
  let functions () : Environment.fn_map =
    let fns : Environment.built_in_fn list =
      [ { name = FnDesc.stdFnDesc "Int" "range" 0
        ; parameters =
            [ param "list" (TList (TVariable "a")) "The list to be operated on"
            ; param
                "fn"
                (TFn ([TVariable "a"], TVariable "b"))
                "Function to be called on each member" ]
        ; return_val =
            ret_val
              (TList TInt)
              "List of ints between lowerBound and upperBound"
        ; fn =
            (function
            | _, [DInt lower; DInt upper] ->
                List.range'
                  ~compare:Z.compare
                  ~stride:Z.succ
                  ~start:`inclusive
                  ~stop:`inclusive
                  lower
                  upper
                |> List.map ~f:(fun i -> DInt i)
                |> DList
                |> Ok
            | _ ->
                Error ()) }
      ; { name = FnDesc.stdFnDesc "List" "map" 0
        ; parameters =
            [ param "list" (TList (TVariable "a")) "The list to be operated on"
            ; param
                "fn"
                (TFn ([TVariable "a"], TVariable "b"))
                "Function to be called on each member" ]
        ; return_val =
            ret_val
              (TList (TVariable "b"))
              "A list created by the elements of `list` with `fn` called on each of them in order"
        ; fn =
            (function
            | env, [DList l; DLambda (st, [var], body)] ->
                let result =
                  l
                  |> List.map ~f:(fun dv ->
                         let st = Map.set st var dv in
                         eval env st body)
                in

                result |> Dval.toDList |> Ok
            | _ ->
                Error ()) }
      ; { name = FnDesc.stdFnDesc "Int" "%" 0
        ; parameters = [param "a" TInt "Numerator"; param "b" TInt "Denominator"]
        ; return_val = ret_val TInt "Returns the modulus of a / b"
        ; fn =
            (function
            | env, [DInt a; DInt b] ->
              (try Ok (DInt (Z.erem a b)) with _ -> Ok (DInt Z.zero))
            | _ ->
                Error ()) }
      ; { name = FnDesc.stdFnDesc "Int" "==" 0
        ; parameters = [param "a" TInt "a"; param "b" TInt "b"]
        ; return_val =
            ret_val
              TBool
              "True if structurally equal (they do not have to be the same piece of memory, two dicts or lists or strings with the same value will be equal), false otherwise"
        ; fn =
            (function
            | env, [DInt a; DInt b] -> Ok (DBool (Z.equal a b)) | _ -> Error ())
        }
      ; { name = FnDesc.stdFnDesc "Int" "toString" 0
        ; parameters = [param "a" TInt "value"]
        ; return_val = ret_val TString "Stringified version of a"
        ; fn =
            (function
            | env, [DInt a] -> Ok (DString (Z.to_string a)) | _ -> Error ()) }
      ; { name = FnDesc.stdFnDesc "HttpClient" "get" 0
        ; parameters = [param "url" TString "The URL to be fetched"]
        ; return_val = ret_val TString "Fetch the body of the page at the URL"
        ; fn =
            (function
            | env, [DString url] ->
              ( try
                  let errorbuf = ref "" in
                  let responsebuf = Buffer.create 16384 in
                  let responsefn str : int =
                    Buffer.add_string responsebuf str ;
                    String.length str
                  in
                  let c = C.init () in
                  C.set_url c url ;
                  C.set_verbose c false ;
                  C.set_errorbuffer c errorbuf ;
                  C.set_followlocation c true ;
                  C.set_failonerror c false ;
                  C.set_writefunction c responsefn ;
                  C.setopt c (Curl.CURLOPT_TIMEOUT 30)
                  (* timeout is infinite by default *) ;
                  (* This tells CURL to send an Accept-Encoding header including all
        * of the encodings it supports *and* tells it to automagically decode
        * responses in those encodings. This works even if someone manually specifies
        * the encoding in the header, as libcurl will still appropriately decode it
        *
        * https://curl.haxx.se/libcurl/c/CURLOPT_ACCEPT_ENCODING.html
        * *)
                  C.set_encoding c C.CURL_ENCODING_ANY ;
                  (* Don't let users curl to e.g. file://; just HTTP and HTTPs. *)
                  C.set_protocols c [C.CURLPROTO_HTTP; C.CURLPROTO_HTTPS] ;
                  (* Seems like redirects can be used to get around the above list... *)
                  C.set_redirprotocols c [C.CURLPROTO_HTTP; C.CURLPROTO_HTTPS] ;
                  (* Actually do the request *)
                  C.perform c ;
                  (* If we get a redirect back, then we may see the content-type
        * header twice. Fortunately, because we push headers to the front
        * above, and take the first in charset, we get the right
        * answer. Whew. To do this correctly, we'd have to implement our
        * own follow logic which would clear the header ref, which seems
        * straightforward in theory but likely not practice.
        * Alternatively, we could clear the headers ref when we receive a
        * new `ok` header. *)
                  C.cleanup c ;
                  responsebuf
                  |> Buffer.contents
                  |> Yojson.Safe.from_string
                  |> Yojson.Safe.Util.member "data"
                  |> Yojson.Safe.Util.to_string
                  |> DString
                  |> Ok
                with Curl.CurlException (curl_code, code, s) -> Error () )
            | _ ->
                Error ()) } ]
    in

    fns
    |> List.map ~f:(fun fn -> (fn.name, fn))
    |> Map.of_alist_exn (module FnDesc)
end

let run (e : expr) : dval =
  let env = Environment.envWith (StdLib.functions ()) in
  eval env Symtable.empty e


let run_json (e : expr) : string =
  run e |> Dval.to_json |> Yojson.Safe.to_string ~std:true
