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
    let result = Interpreter.runString program
    return! OK result x }

let app =
    choose [ GET >=> (path "/" >=> (runProgram ()) )]

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()

    let conf =
        { defaultConfig with
              cancellationToken = cts.Token }

    let listening, server = startWebServerAsync conf app

    Async.Start(server, cts.Token)
    printfn "Make requests now"
    Console.ReadKey true |> ignore

    cts.Cancel()

    0 // return an integer exit code
