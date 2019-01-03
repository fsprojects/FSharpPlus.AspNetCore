module FSharpPlus.AspNetCore.Suave
open FSharpPlus
open FSharpPlus.Data
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open System.Net
open System.Text

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
    type Response = { statusCode : int option; content: Choice<string,byte array> option; contentType:string option}

    type Context = { request:HttpRequest; response:Response }
    module Response=
        let empty = { statusCode=None; content=None; contentType=None }

    module Context=
        let ofHttpContext (httpContext:HttpContext)=
            { request = httpContext.Request; response = Response.empty }
        let response statusCode content=
            OptionT << fun ctx -> { ctx with response = { ctx.response with statusCode = Some statusCode; content=Choice1Of2 content |> Some }} |> succeed

    let yieldToResponse (from:Response) (to':HttpResponse)=
        match from.contentType with | Some contentType -> to'.ContentType <- contentType | _ -> ()
        match from.statusCode with | Some statusCode -> to'.StatusCode <- statusCode | _ -> ()
        match from.content with
        | Some (Choice1Of2 content) -> to'.WriteAsync(content)
        | Some (Choice2Of2 content) -> to'.WriteAsync(Encoding.UTF8.GetString(content), Encoding.UTF8)
        | _ -> Task.CompletedTask
open Http
module Successful=
    let OK s = Context.response (int HttpStatusCode.OK) s
    let ACCEPTED s = Context.response (int HttpStatusCode.Accepted) s
    let CREATED s = Context.response (int HttpStatusCode.Created) s
    let NO_CONTENT s = Context.response (int HttpStatusCode.NoContent) s
module RequestErrors=
    let BAD_REQUEST s = Context.response (int HttpStatusCode.BadRequest) s
    let NOT_ACCEPTABLE s = Context.response (int HttpStatusCode.NotAcceptable) s
    let METHOD_NOT_ALLOWED s = Context.response (int HttpStatusCode.MethodNotAllowed) s
    let FORBIDDEN s = Context.response (int HttpStatusCode.Forbidden) s
    let NOT_FOUND s = Context.response (int HttpStatusCode.NotFound) s
    let UNAUTHORIZED s = Context.response (int HttpStatusCode.Unauthorized) s
module Filters=
    let response (method : string) =
        OptionT << fun (x : Context) -> async.Return (if (method = x.request.Method) then Some x else None)
    let hasFormContentType =
        OptionT << fun (x : Context) -> async.Return (if x.request.HasFormContentType then Some x else None)

    let GET  (x : Http.Context) =  response "GET" x
    let POST (x : Http.Context) = response "POST" x
    let DELETE (x : Http.Context) = response "DELETE" x
    let HEAD (x : Http.Context) = response "HEAD" x
    let OPTIONS (x : Http.Context) = response "OPTIONS" x
    let PATCH (x : Http.Context) = response "PATCH" x
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
open FSharp.Control.Tasks.V2
let appRun (app:WebPart<Context>) (appBuilder:IApplicationBuilder)=
    let appRun (func:HttpContext->#Task) (b: IApplicationBuilder) =
        b.Run(RequestDelegate(fun ctx->func ctx :> Task))

    let runApp context = task {
        let ctx = Context.ofHttpContext context
        let! task = app ctx |> OptionT.run |> Async.StartAsTask
        match task with
        | Some res-> return! Http.yieldToResponse res.response context.Response
        | None -> return! Task.CompletedTask
    }
    appRun runApp appBuilder

