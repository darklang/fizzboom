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

let runFizzbuzz =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let fizzbuzz = Interpreter.runJSON Interpreter.fizzbuzz
        text fizzbuzz next ctx

let runFizzboom =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let fizzboom = Interpreter.runJSON Interpreter.fizzboom
        text fizzboom next ctx

let webApp =
    choose [ GET >=> choose [
        route "/fizzbuzz" >=> runFizzbuzz
        route "/fizzboom" >=> runFizzboom ]]

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