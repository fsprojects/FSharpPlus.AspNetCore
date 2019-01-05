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

open FSharp.Control.Tasks.V2

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

type Note = { id: int; text: string }
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
  abstract member GetUserNotes: int -> Async<NoteList>
  abstract member AddUserNote: int -> string -> Async<Note>
  abstract member GetNote: int ->Async<Note option>

let webApp (db: IDb) =
  let overview =
    GET >=> (authenticated <| fun ctx userId -> monad {
      let! res = lift (db.GetUserNotes userId)
      let ovm = toJson res |> string
      return! Json.OK ovm ctx
    })
  let getNote (id:int)=
    GET >=> (fun ctx -> monad {
      let! maybeNote = lift (db.GetNote id)
      return! Json.OK (toJson maybeNote) ctx
    })
  let getNotePart (id:int) (part:int)=
    GET >=> (fun ctx -> monad {
      let! maybeNote = lift (db.GetNote id)
      let json = maybeNote |> map(fun (n:Note)-> n.text.Substring(0,part)) |> toJson
      return! Json.OK json ctx
    })
  let register =
    POST >=> hasFormContentType >=> (authenticated <| fun ctx userId -> monad {
      match ctx.request |> Request.Form.tryGet "text" with
      | Some text ->
        let! newNote = lift (db.AddUserNote userId (string text))
        return! Json.CREATED (toJson newNote) ctx
      | None ->
        return! BAD_REQUEST "Could not find text" ctx
    })

  let v1=
    WebPart.choose [ path "/" >=> (OK "/")
                     path "/notes" >=> register
                     pathRegex "/notes/(\\d+)" getNote
                     pathRegex2 "/notes/(\\d+)/_/(\\d+)" getNotePart
                     path "/notes" >=> overview ]
  let v2=
    WebPart.choose [ path "/" >=> (OK "/")
                     path "/notes" >=> register
                     pathScan "/notes/%d" getNote
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
      let mutable notes = []
      {new IDb with
       member __.GetUserNotes userId =
        let notes = List.filter ((=) userId << fst) notes |> List.map snd
        async{ return { notes = notes; offset = 0; chunk = 100; total=notes.Length } }

       member __.AddUserNote userId note=
        let note = {id=notes.Length+1; text=note}
        notes<-(userId,note) :: notes
        async { return note }
       member __.GetNote id=
        let note = List.tryFind ((=) id << Note.id<< snd) notes |> Option.map snd
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
    client.PostAsync("http://localhost"+path, new FormUrlEncodedContent(content |> List.map (fun (k,v)-> KeyValuePair.Create(k,v))))
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
        Expect.equal (parseJson noteJson) (Ok {id=1;text="my text"}) "Expected note result!"
      })
      testCase "Read all notes" <| fun _ ->waitFor(task {
        use testServer = TestServer.create()
        use client = testServer.CreateClient()
        let! _ = client |> postForm notesUrl [("text","my text")]
        let! resp= client |> get notesUrl
        let! notesJson = resp.Content.ReadAsStringAsync()
        Expect.equal (parseJson notesJson) (Ok {notes=[{id=1;text="my text"}];chunk=100; offset=0;total=1}) "Expected notes json"
      })
      testCase "Read a single note" <| fun _ ->waitFor(task {
        use testServer = TestServer.create()
        use client = testServer.CreateClient()
        let! _ = client |> postForm notesUrl [("text","my text")]
        let! resp= client |> get (notesUrl+"/1")
        let! noteJson = resp.Content.ReadAsStringAsync()
        Expect.equal (parseJson noteJson) (Ok {id=1;text="my text"}) "Expected note json"
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
  let testsV2 = testList "integration test api v2" <| testFixture 2
