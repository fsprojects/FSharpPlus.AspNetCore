﻿module FSharpPlus.AspNetCore.Suave
open FSharpPlus
open FSharpPlus.Data
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open System.Net
open System.Text
open System
open System.IO
open System.Text.RegularExpressions

// setup something that reminds us of what Suave can work with
// this is an overly simplified model of Suave in order to show how OptionT can be used
// in conjunction with generic Kleisli composition (fish) operator
type WebPart<'a> = 'a -> OptionT<Async<'a option>>

type HeaderKey(key: string) =
  let lower = key.ToLowerInvariant()
  member __.ToLowerInvariant() = lower
  override __.ToString() = key
  override this.GetHashCode() = hash lower
  override this.Equals(obj:obj) =  match obj with | :? HeaderKey as k -> this.ToLowerInvariant().Equals(k.ToLowerInvariant()) | _ -> false
  interface IComparable with
    member this.CompareTo(otherObject) =
      let other = otherObject :?> HeaderKey
      this.ToLowerInvariant().CompareTo(other.ToLowerInvariant())
module WebPart=
  /// Comment from <a href="https://github.com/SuaveIO/suave/blob/v2.4.3/src/Suave/WebPart.fsi#L39-L42">WebPart.fsi</a>
  /// Entry-point for composing the applicative routes of the http application,
  /// by iterating the options, applying the context, arg, to the predicate
  /// from the list of options, until there's a match/a Some(x) which can be
  /// run.
  [<Obsolete("Use choice")>]
  let choose (options : WebPart<'a> list) = fun x -> choice (List.map ((|>) x) options)
  let inline fail (_:'a) : OptionT<Async<'a option>> = OptionT <| async.Return None

module Http=
  type Response = {
    statusCode : int option;
    content: string option;
    contentType:string option;
    headers: Map<HeaderKey,string>
  }

  type Context = { request:HttpRequest; response:Response }
  module Response=
    let empty = { statusCode=None; content=None; contentType=None ; headers=Map.empty}
  module Context=
    let ofHttpContext (httpContext:HttpContext)=
      { request = httpContext.Request; response = Response.empty }
  let yieldToResponse (from:Response) (to':HttpResponse)=
    match from.contentType with | Some contentType -> to'.ContentType <- contentType | _ -> ()
    match from.statusCode with | Some statusCode -> to'.StatusCode <- statusCode | _ -> ()
    match from.content with
    | Some content -> to'.WriteAsync(content)
    | _ -> Task.CompletedTask
open Http
module Writers=
  let private succeed x = async.Return (Some x)

  let setStatusAndContent statusCode content=
    OptionT << fun ctx -> { ctx with response = { ctx.response with statusCode = Some statusCode; content=Some content }} |> succeed
  let inline _response f (ctx:Context) = map (fun a' -> { ctx with response=a' }) (f ctx.response)
  let inline _request f ctx = map (fun a' -> { ctx with request=a' }) (f ctx.request)
  module Response =
    let inline _status f (resp:Http.Response) = map (fun a' -> { resp with statusCode=a' }) (f resp.statusCode)
    let inline _headers f (resp:Http.Response) = map (fun a' -> { resp with headers=a' }) (f resp.headers)
    let inline _content f (resp:Http.Response) = map (fun a' -> { resp with content=a' }) (f resp.content)

  let setHeader key value =
    OptionT << fun ctx -> { ctx with response = { ctx.response with headers = Map.add (HeaderKey key) value ctx.response.headers }} |> succeed

  let setContentType contentType =
    OptionT << fun ctx -> { ctx with response = { ctx.response with contentType = Some contentType }} |> succeed

open Writers
module Successful=
  let OK s = setStatusAndContent (int HttpStatusCode.OK) s
  let ACCEPTED s = setStatusAndContent (int HttpStatusCode.Accepted) s
  let CREATED s = setStatusAndContent (int HttpStatusCode.Created) s
  let NO_CONTENT s = setStatusAndContent (int HttpStatusCode.NoContent) s
module RequestErrors=
  let BAD_REQUEST s = setStatusAndContent (int HttpStatusCode.BadRequest) s
  let NOT_ACCEPTABLE s = setStatusAndContent (int HttpStatusCode.NotAcceptable) s
  let METHOD_NOT_ALLOWED s = setStatusAndContent (int HttpStatusCode.MethodNotAllowed) s
  let FORBIDDEN s = setStatusAndContent (int HttpStatusCode.Forbidden) s
  let NOT_FOUND s = setStatusAndContent (int HttpStatusCode.NotFound) s
  let UNAUTHORIZED s = setStatusAndContent (int HttpStatusCode.Unauthorized) s
module Filters=
  let response (method : string) = OptionT << fun (x : Context) -> async.Return (if (method = x.request.Method) then Some x else None)
  let hasFormContentType = OptionT << fun (x : Context) -> async.Return (if x.request.HasFormContentType then Some x else None)

  let GET  (x : Http.Context) =  response "GET" x
  let POST (x : Http.Context) = response "POST" x
  let PUT (x : Http.Context) = response "PUT" x
  let DELETE (x : Http.Context) = response "DELETE" x
  let HEAD (x : Http.Context) = response "HEAD" x
  let OPTIONS (x : Http.Context) = response "OPTIONS" x
  let PATCH (x : Http.Context) = response "PATCH" x
  let path s = let path = implicit s in OptionT << fun (x : Http.Context) -> async.Return (if (path = x.request.Path) then Some x else None)

  let inline pathScan (path) (routeHandler) : WebPart<Context>=
    fun (x : Http.Context) ->
      match string x.request.Path |> trySscanf path with
      | Some p ->routeHandler p x
      | _ -> WebPart.fail x

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
  module Header=
    let tryGet key (r:HttpRequest)=match r.Headers.TryGetValue key with | (true,v)->Some v | _-> None



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

