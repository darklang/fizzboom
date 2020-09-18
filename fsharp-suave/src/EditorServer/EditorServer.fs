open System
open System.Threading
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open LibExecution

let app =
    choose [ GET >=> choose [
      (path "/fizzbuzz" >=> request (fun r -> OK (Interpreter.runJSON Interpreter.fizzbuzz) ))
      (path "/fizzboom" >=> request (fun r -> OK (Interpreter.runJSON Interpreter.fizzboom) )) ]]

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
