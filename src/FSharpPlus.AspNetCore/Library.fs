namespace FSharpPlus.AspNetCore
open FSharpPlus
open FSharpPlus.Data
open System
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open System.Threading.Tasks

module HttpAdapter=
  let appMap (path: string) (map : IApplicationBuilder->unit) (app: IApplicationBuilder): IApplicationBuilder =
    app.Map(PathString(path), Action<_>(map))
  let appRun (func:HttpContext->#Task) (b: IApplicationBuilder) =
    b.Run(RequestDelegate(fun ctx->func ctx :> Task))
  let appMapWhen (when':HttpContext->bool) then' (b: IApplicationBuilder) =
    b.MapWhen(Func<_,_>(when'), Action<_>(then'))
