open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open LibExecution
open FSharp.Control.Tasks

let runSync e =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let fizzbuzz = Interpreter.runJSON e
        text fizzbuzz next ctx

let runAsync e =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let! fizzboom = Interpreter.runJSONAsync e
            return! text fizzboom next ctx
        }

let run e =
    if Interpreter.requiresAsync e then
        runAsync e
    else
        runSync e

let webApp =
    choose [ GET >=> choose [
        route "/fizzbuzz" >=> run Interpreter.fizzbuzz
        route "/fizzboom" >=> run Interpreter.fizzboom ]]

let configureApp (app : IApplicationBuilder) =
    app.UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddFilter(fun l -> l >= LogLevel.Debug)
           .AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let port = int (System.IO.File.ReadAllText("port"))
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseUrls(sprintf "http://127.0.0.1:%i" port)
                    .UseContentRoot(contentRoot)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    |> ignore)
        .Build()
        .Run()
    0