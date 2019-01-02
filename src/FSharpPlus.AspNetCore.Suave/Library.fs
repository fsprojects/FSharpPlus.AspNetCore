module FSharpPlus.AspNetCore.Suave
open FSharpPlus.AspNetCore
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

// setup something that reminds us of what Suave can work with
// this is an overly simplified model of Suave in order to show how OptionT can be used
// in conjunction with generic Kleisli composition (fish) operator
type WebPart<'a> = 'a -> OptionT<Async<'a option>>
let inline succeed x = async.Return (Some x)

module WebPart=
    /// Comment from <a href="https://github.com/SuaveIO/suave/blob/v2.4.3/src/Suave/WebPart.fsi#L39-L42">WebPart.fsi</a>
    /// Entry-point for composing the applicative routes of the http application,
    /// by iterating the options, applying the context, arg, to the predicate
    /// from the list of options, until there's a match/a Some(x) which can be
    /// run.
    let choose (options : WebPart<'a> list) = fun x -> choice (List.map ((|>) x) options)

module Http=
    type Response = { statusCode : int option; content:string option; contentType:string option}
    module Response=
        let empty = { statusCode=None; content=None; contentType=None }
    //type HttpRequest = { url : Uri; ``method``:string }
    type Context = { request:HttpRequest; response:Response }
    module Context=
        let ofHttpContext (httpContext:HttpContext)=
            { request = httpContext.Request; response = Response.empty }
    let yieldToResponse (from:Response) (to':HttpResponse)=
        match from.contentType with | Some contentType -> to'.ContentType <- contentType | _ -> ()
        match from.statusCode with | Some statusCode -> to'.StatusCode <- statusCode | _ -> ()
        match from.content with | Some content -> to'.WriteAsync(content) | _ -> Task.CompletedTask

module Successful=
    open Http
    let private withStatusCode statusCode s=
        OptionT << fun ctx -> { ctx with response = { ctx.response with statusCode = Some statusCode; content = Some s }} |> succeed
    let OK s = withStatusCode 200 s
    let BAD_REQUEST s = withStatusCode 400 s

module Filters=
    open Http
    let ``method`` (m : string) =
        OptionT << fun (x : Http.Context) -> async.Return (if (m = x.request.Method) then Some x else None)
    let GET  (x : Http.Context) = ``method`` "GET" x
    let POST (x : Http.Context) = ``method`` "POST" x
    let DELETE (x : Http.Context) = ``method`` "DELETE" x
    let HEAD (x : Http.Context) = ``method`` "HEAD" x
    let OPTIONS (x : Http.Context) = ``method`` "OPTIONS" x
    let PATCH (x : Http.Context) = ``method`` "PATCH" x
    let path s =
        let path = implicit s
        OptionT << fun (x : Http.Context) -> async.Return (if (path = x.request.Path) then Some x else None)

module Request =
    module Form=
        let tryGet name (request: HttpRequest) =
            match request.Form.TryGetValue name with
            | true, v -> Some v
            | _       -> None
    module Query=
        let tryGet name (request: HttpRequest) =
            match request.Query.TryGetValue name with
            | true, v -> Some v
            | _       -> None
let yieldTo (from:Http.Context) (to':HttpContext)= Http.yieldToResponse from.response to'.Response
open Http
open FSharp.Control.Tasks.V2
let appRun (app:WebPart<Context>) (appBuilder:IApplicationBuilder)=
    let runApp context = task {
        let ctx = Context.ofHttpContext context
        let task = app ctx |> OptionT.run |> Async.StartAsTask
        match! task with | Some res-> return! yieldTo res context | None -> return! Task.CompletedTask
    }
    HttpAdapter.appRun runApp appBuilder

