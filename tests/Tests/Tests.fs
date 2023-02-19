module Tests

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
open Microsoft.AspNetCore.TestHost

open Expecto

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

module ``integration test using test server`` =
  module TestServer=
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
    let create ()=
      let svc = fakeDb()
      let builder = WebHostBuilder()
                      .Configure(fun app->HttpAdapter.configuration svc app)
      new TestServer(builder)
  let waitFor (t:Task)= t.Wait()
  //echo '{"sub":"1"}' | base64
  let base64Encode : string -> string = Encoding.UTF8.GetBytes>>Convert.ToBase64String
  let setAuth (client:HttpClient) =
    client.DefaultRequestHeaders.Remove "x-jwt-payload" |> ignore
    client.DefaultRequestHeaders.Add("x-jwt-payload", base64Encode "{\"sub\":\"1\"}")
  let postJson path content (client:HttpClient)=
    setAuth client
    client.PostAsync("http://localhost"+path, new StringContent(content,Encoding.UTF8,"application/json"))
  let postForm path content (client:HttpClient)=
    setAuth client
    client.PostAsync("http://localhost"+path, new FormUrlEncodedContent(content |> List.map KeyValuePair.Create))
  let putForm path content (client:HttpClient)=
    setAuth client
    client.PutAsync("http://localhost"+path, new FormUrlEncodedContent(content |> List.map KeyValuePair.Create))
  let get path (client:HttpClient)=
    setAuth client
    client.GetAsync("http://localhost"+path)
  let testFixture version=
    let notesUrl = (sprintf "/v%d/notes" version)
    [
      testCase "Add note" <| fun _ ->waitFor(task {
        use testServer = TestServer.create()
        use client = testServer.CreateClient()
        let! noteRes= client |> postForm notesUrl [("text","my text")]
        Expect.equal noteRes.StatusCode System.Net.HttpStatusCode.Created "Expected created status code"
        let! noteJson = noteRes.Content.ReadAsStringAsync()
        Expect.equal (parseJson noteJson) (Ok {id=NoteId 1;text="my text"}) "Expected note result!"
      })
      testCase "Read all notes" <| fun _ ->waitFor(task {
        use testServer = TestServer.create()
        use client = testServer.CreateClient()
        let! _ = client |> postForm notesUrl [("text","my text")]
        let! resp= client |> get notesUrl
        let! notesJson = resp.Content.ReadAsStringAsync()
        Expect.equal (parseJson notesJson) (Ok {notes=[{id=NoteId 1;text="my text"}];chunk=100; offset=0;total=1}) "Expected notes json"
      })
      testCase "Read a single note" <| fun _ ->waitFor(task {
        use testServer = TestServer.create()
        use client = testServer.CreateClient()
        let! _ = client |> postForm notesUrl [("text","my text")]
        let! resp= client |> get (notesUrl+"/1")
        let! noteJson = resp.Content.ReadAsStringAsync()
        Expect.equal (parseJson noteJson) (Ok {id=NoteId 1;text="my text"}) "Expected note json"
      })
    ]
  [<Tests>]
  let testsV1 = testList "integration test api v1" <| testFixture 1 @ [
    testCase "Read a part of a note" <| fun _ ->waitFor(task {
      use testServer = TestServer.create()
      use client = testServer.CreateClient()
      let! _ = client |> postForm "/v1/notes" [("text","my text")]
      let! resp= client |> get "/v1/notes/1/_/2"
      let! noteJson = resp.Content.ReadAsStringAsync()
      Expect.equal noteJson "\"my\"" "Expected text"
    })
  ]

  [<Tests>]
  let testsV2 = testList "integration test api v2" <| testFixture 2 @ [
    testCase "Update a note" <| fun _ ->waitFor(task {
      use testServer = TestServer.create()
      use client = testServer.CreateClient()
      let! _ = client |> postForm "/v2/notes" [("text","my text")]
      let! resp= client |> putForm "/v2/notes/1" [("text","my next text")]
      let! noteJson = resp.Content.ReadAsStringAsync()
      Expect.equal (parseJson noteJson) (Ok {id=NoteId 1;text="my next text"}) "Expected note json"
    })
  ]
