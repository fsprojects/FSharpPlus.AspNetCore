module Notes
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open System
open System.Net.Http
open System.Text
open System.Collections.Generic
open System.Threading.Tasks

open FSharpPlus
open FSharpPlus.Data

open Fleece.FSharpData
open Fleece.FSharpData.Operators

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http

open FSharpPlus.AspNetCore
open FSharpPlus.AspNetCore.Suave
open HttpAdapter
open Successful
open RequestErrors
open Filters
open Writers
type JwtPayload = { subject:string }
with
  static member OfJson json:ParseResult<JwtPayload> =
    let create sub= { subject =sub }
    match json with
    | JObject o -> create <!> (o .@ "sub")
    | x -> Decode.Fail.objExpected x
let authenticated f =
  fun (ctx:Http.Context) ->
    match Request.Header.tryGet "x-jwt-payload" ctx.request with
    | Some u ->
      string u
      |> Convert.FromBase64String
      |> Encoding.UTF8.GetString
      |> parseJson
      |> Result.map( fun (payload:JwtPayload)->tryParse payload.subject)
      |> function | Ok (Some user)-> f ctx user
                  | _ -> UNAUTHORIZED "" ctx
    | None -> UNAUTHORIZED "" ctx

module Json=
  let inline OK v=
    OK (string v)
    >=> setContentType "application/json; charset=utf-8"
  let inline CREATED v=
    CREATED (string v)
    >=> setContentType "application/json; charset=utf-8"
  let inline BAD_REQUEST v =
    BAD_REQUEST (string v)
    >=> setContentType "application/json; charset=utf-8"
[<Struct>]
type UserId = UserId of string
with
  override this.ToString()=match this with UserId uId->uId
[<Struct>]
type NoteId = NoteId of int
with
  override this.ToString()=match this with NoteId nId -> nId.ToString()
  static member ToJson (NoteId x) = JNumber (decimal x)
  static member OfJson json = ofJson json |> Result.map NoteId
type Note = { id: NoteId; text: string }

type Note with
  static member JsonObjCodec =
    fun id text -> { id = id; text = text  }
    <!> jreq  "id"          (fun x -> Some x.id    )
    <*> jreq  "text"        (fun x -> Some x.text  )
    |> Codec.ofConcrete
module Note=
  let id (n:Note)=n.id
type NoteList = { notes: Note list; offset: int; chunk: int; total: int }
type NoteList with
  static member JsonObjCodec =
    fun notes offset chunk total -> { notes = notes; offset = offset; chunk = chunk; total=total  }
    <!> jreq  "notes"          (fun x -> Some x.notes     )
    <*> jreq  "offset"         (fun x -> Some x.offset    )
    <*> jreq  "chunk"          (fun x -> Some x.chunk     )
    <*> jreq  "total"          (fun x -> Some x.total     )
    |> Codec.ofConcrete

type IDb =
  abstract member GetUserNotes: UserId -> Async<NoteList>
  abstract member AddUserNote: UserId -> string -> Async<Note>
  abstract member GetNote: NoteId ->Async<Note option>
  abstract member UpdateUserNote: UserId -> NoteId -> string -> Async<Note option>

let webApp (db: IDb) =
  let overview =
    GET >=> (authenticated <| fun ctx userId -> monad {
      let! res = lift (db.GetUserNotes <| UserId userId)
      let ovm = toJson res |> string
      return! Json.OK ovm ctx
    })
  let getNote (id:int)=
    GET >=> (fun ctx -> monad {
      let! maybeNote = lift (db.GetNote <| NoteId id)
      return! Json.OK (toJson maybeNote) ctx
    })
  let getNotePart (id:int, part:int)=
    GET >=> (fun ctx -> monad {
      let! maybeNote = lift (db.GetNote <| NoteId id)
      let json = maybeNote |> map(fun (n:Note)-> n.text.Substring(0,part)) |> toJson
      return! Json.OK json ctx
    })
  let register =
    POST >=> hasFormContentType >=> (authenticated <| fun ctx userId -> monad {
      match ctx.request |> Request.Form.tryGet "text" with
      | Some text ->
        let! newNote = lift (db.AddUserNote (UserId userId) (string text))
        return! Json.CREATED (toJson newNote) ctx
      | None ->
        return! BAD_REQUEST "Could not find text" ctx
    })
  let updateNote (id:int) =
    PUT >=> hasFormContentType >=> (authenticated <| fun ctx userId -> monad {
      match ctx.request |> Request.Form.tryGet "text" with
      | Some text ->
        match! lift (db.UpdateUserNote (UserId userId) (NoteId id) (string text)) with
        | Some note-> return! Json.OK (toJson note) ctx
        | None -> return! NOT_FOUND "Could not find note" ctx
      | None ->
        return! BAD_REQUEST "Could not find text" ctx
    })
  let v1=
    WebPart.choose [ path "/" >=> (OK "/")
                     path "/notes" >=> register
                     pathScan "/notes/%d" getNote
                     pathScan "/notes/%d/_/%d" getNotePart
                     path "/notes" >=> overview ]
  let v2=
    WebPart.choose [ path "/" >=> (OK "/")
                     path "/notes" >=> register
                     pathScan "/notes/%d" getNote
                     pathScan "/notes/%d" updateNote
                     path "/notes" >=> overview ]
  (v1,v2)
module HttpAdapter=
  let indexHtml = """
<!DOCTYPE html>
<html>
<head>
    <title>Some service</title>
    <meta charset="utf-8" />
</head>
<body>
</body>
</html>
"""


  let index(context: HttpContext) : Task =
    context.Response.ContentType <- "text/html; charset=utf-8"
    //Console.WriteLine("Index")
    context.Response.WriteAsync(indexHtml)
  let configuration (svc: IDb) (app: IApplicationBuilder)=
    let (webAppV1,webAppV2)=webApp svc
    app
      |> appMap "/index.html" (appRun index)
      |> appMap "/v1" (Suave.appRun webAppV1)
      |> appMap "/v2" (Suave.appRun webAppV2)
      |> ignore
[<EntryPoint>]
let main argv =
  let builder = WebApplication.CreateBuilder(argv);

  let app = builder.Build()
  let fakeDb() =
    let withUserId userId = (=) userId << fst
    let withId id = (=) id << Note.id<< snd
    let mutable notes = []
    {new IDb with
     member __.GetUserNotes userId =
      let notes = List.filter (withUserId userId) notes |> List.map snd
      async{ return { notes = notes; offset = 0; chunk = 100; total=notes.Length } }

     member __.AddUserNote userId text=
      let note = {id=NoteId <| notes.Length+1; text=text}
      notes<-(userId,note) :: notes
      async { return note }
     member __.UpdateUserNote userId id text=
      let maybeNote = List.tryFind (withId id) notes |> Option.map snd
      match maybeNote with
      | Some note->
        let nextNote = {note with text=text }
        notes<-(userId,note) :: List.filter (not << withId id) notes
        async.Return <| Some nextNote
      | None -> async.Return None
     member __.GetNote id=
      let note = List.tryFind (withId id) notes |> Option.map snd
      async { return note }
    }

  HttpAdapter.configuration (fakeDb()) app

  // Configure the HTTP request pipeline.
  if (not <| app.Environment.IsDevelopment()) then
      app.UseExceptionHandler("/Error") |> ignore
      // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
      app.UseHsts() |> ignore

  app.UseHttpsRedirection() |> ignore

  app.UseRouting() |> ignore

  app.UseAuthorization() |> ignore

  app.Run()
  0
