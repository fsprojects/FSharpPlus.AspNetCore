namespace Todos
#nowarn "20"
open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.HttpsPolicy
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Swashbuckle
open Swashbuckle.AspNetCore
open Swashbuckle.AspNetCore.Annotations
open Swashbuckle.AspNetCore.Swagger
open Microsoft.AspNetCore.Http

type TodoStatus = ``waiting`` = 0 | ``working`` = 1 | ``done`` = 2
type Todo = {id:int64; title:string; create_date: DateTime; done_date: Nullable<DateTime>; status: TodoStatus}
module Todos=
  let register (app:WebApplication) =
        app.MapGet("/todos",handler=Func<_>(fun ()->Task.CompletedTask))
           .WithMetadata(
            SwaggerOperationAttribute(summary="get all todos"),
            SwaggerResponseAttribute(200, "success", typeof<Todo>))
           .WithName("GetTodos")
           .WithOpenApi()

        app.MapPost("/todos",handler=Func<_>(fun r->Task.CompletedTask))
           .WithMetadata(
            SwaggerOperationAttribute(summary="create todo"),
            SwaggerResponseAttribute(200, "success", typeof<Todo>),
            SwaggerResponseAttribute(400, ""))
           .WithName("PostTodo")
           .WithOpenApi().Accepts<Todo>("application/json")

module WeatherForecasts=
  type WeatherForecast(Date:DateTime, TemperatureC: int, Summary:String) =
    member _.TemperatureF = 32 + int((float TemperatureC) / 0.5556)
  // https://learn.microsoft.com/en-us/aspnet/core/fundamentals/minimal-apis/openapi?view=aspnetcore-7.0
  let register (app:WebApplication) =
        let summaries = ["Freezing"; "Bracing"; "Chilly"; "Cool"; "Mild"; "Warm"; "Balmy"; "Hot"; "Sweltering"; "Scorching"]
        let forecast (index:int) = WeatherForecast (
          DateTime.Now.AddDays(index),
          Random.Shared.Next(-20, 55),
          summaries[Random.Shared.Next(summaries.Length)] )
        let weatherForecast () = seq { 1 .. 5 } |> Seq.map forecast
        app.MapGet("/weatherforecast", handler=Func<_>( weatherForecast ))
           .WithName("GetWeatherForecast").WithOpenApi()

module Program =
    let exitCode = 0

    [<EntryPoint>]
    let main args =

        let builder = WebApplication.CreateBuilder(args)

        builder.Services.AddEndpointsApiExplorer()
        builder.Services.AddControllers()
        builder.Services.AddSwaggerGen(fun c->c.EnableAnnotations())

        let app = builder.Build()

        app.MapControllers();
        app.UseHttpsRedirection()


        app.UseSwagger()
        app.UseSwaggerUI()
        Todos.register app
        WeatherForecasts.register app
        app.Run()

        exitCode
