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
  [<Obsolete("Prefer app.Map")>]
  let appMap (path: string) (map : IApplicationBuilder->unit) (app: IApplicationBuilder): IApplicationBuilder =
    app.Map(PathString(path), Action<_>(map))
  [<Obsolete("Prefer app.Run")>]
  let appRun (func:HttpContext->#Task) (b: IApplicationBuilder) =
    b.Run(RequestDelegate(fun ctx->func ctx :> Task))
  [<Obsolete("Prefer app.MapWhen")>]
  let appMapWhen (when':HttpContext->bool) then' (b: IApplicationBuilder) =
    b.MapWhen(Func<_,_>(when'), Action<_>(then'))
