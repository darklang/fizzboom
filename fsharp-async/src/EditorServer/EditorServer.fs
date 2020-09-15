open System
open System.Threading
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open LibExecution

let runProgram () : WebPart =
  fun ( x : HttpContext) ->
  async {
    let program = Interpreter.program
    let! result = Interpreter.runJSON program
    return! OK result x }

let app =
    choose [ GET >=> (path "/" >=> (runProgram ()) )]

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()
    let port = System.IO.File.ReadAllText("port")

    let conf =
        { defaultConfig with
              bindings = [HttpBinding.createSimple HTTP "127.0.0.1" (int port)]
              cancellationToken = cts.Token }

    let listening, server = startWebServerAsync conf app

    Async.Start(server, cts.Token)
    printfn "Make requests now"
    Console.ReadKey true |> ignore

    cts.Cancel()

    0 // return an integer exit code
