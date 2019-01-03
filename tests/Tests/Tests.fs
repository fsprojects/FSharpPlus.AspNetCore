module Tests

open FSharpPlus
open FSharpPlus.Data
open Expecto
open FSharpPlus.AspNetCore
open FSharpPlus.AspNetCore.Suave
open Fleece.FSharpData
open Fleece.FSharpData.Operators

let authenticated (f: Http.Context -> int -> OptionT<Async<'a option>>) =
    // we assume that authenticated executes f only if auth, otherwise returns 401
    // we fake it as:
    fun (ctx: Http.Context) -> f ctx -1

// Usage:
open Successful
open RequestErrors
open Filters
type Note = { id: int; text: string }
type Note with
    static member JsonObjCodec =
        fun id text -> { id = id; text = text  }
        <!> jreq  "id"          (fun x -> Some x.id    )
        <*> jreq  "text"        (fun x -> Some x.text  )
        |> Codec.ofConcrete
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

let webApp (db: IDb) =
    let overview =
        GET >=> (authenticated <| fun ctx userId ->
            monad {
              let! res = lift (db.GetUserNotes userId)
              let ovm = toJson res |> string
              return! OK ovm ctx
            })
    let register =
        POST >=> hasFormContentType >=> (authenticated <| fun ctx userId ->
            monad {
              match ctx.request |> Request.Form.tryGet "text" with
              | Some text ->
                  let! newNote = lift (db.AddUserNote userId (string text))
                  let rvm = toJson newNote |> string
                  return! CREATED rvm ctx
              | None ->
                  return! BAD_REQUEST "Could not find text" ctx
            })
    WebPart.choose [ path "/" >=> (OK "/")
                     path "/notes" >=> register
                     path "/notes" >=> overview ]
open HttpAdapter
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open System.Threading.Tasks
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open FSharp.Control.Tasks.V2
open System.Net.Http
open System.Text
open FSharp.Data
open System.Collections.Generic

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
        app
            |> appMap "/index.html" (appRun index)
            |> appMap "/api" (Suave.appRun ( webApp svc ))
            |> ignore

module ``integration test using test server`` =
    module TestServer=
        let fakeDb() =
            let mutable notes = []
            { new IDb with
               member __.GetUserNotes userId =
                let notes = List.filter ((=) userId << fst) notes |> List.map snd
                async{ return { notes = notes; offset = 0; chunk = 100; total=notes.Length } }

               member __.AddUserNote userId note=
                let note = {id=notes.Length+1; text=note}
                notes<-(userId,note) :: notes
                async { return note }
            }
        let create ()=
            let svc = fakeDb()
            let builder = WebHostBuilder()
                            .Configure(fun app->HttpAdapter.configuration svc app)
            new TestServer(builder)
    let waitFor (t:Task)= t.Wait()
    let postJson path content (client:HttpClient)=
        client.PostAsync("http://localhost"+path, new StringContent(content,Encoding.UTF8,"application/json"))
    let postForm path content (client:HttpClient)=
        client.PostAsync("http://localhost"+path, new FormUrlEncodedContent(content |> List.map (fun (k,v)-> KeyValuePair.Create(k,v))))
    let get path (client:HttpClient)=
        client.GetAsync("http://localhost"+path)
    [<Tests>]
    let tests =
      testList "integration test api " [
        testCase "Add note" <| fun _ ->waitFor(task {
            use testServer = TestServer.create()
            use client = testServer.CreateClient()
            let! noteRes= client |> postForm "/api/notes" [("text","my text")]
            Expect.equal noteRes.StatusCode System.Net.HttpStatusCode.Created "Expected created status code"
            let! noteJson = noteRes.Content.ReadAsStringAsync()
            Expect.equal (parseJson noteJson) (Ok {id=1;text="my text"}) "Expected note result!"
        })
        testCase "Read all notes" <| fun _ ->waitFor(task {
            use testServer = TestServer.create()
            use client = testServer.CreateClient()
            let! _ = client |> postForm "/api/notes" [("text","my text")]
            let! resp= client |> get "/api/notes"
            let! notesJson = resp.Content.ReadAsStringAsync()
            Expect.equal (parseJson notesJson) (Ok {notes=[{id=1;text="my text"}];chunk=100; offset=0;total=1}) "Expected notes json"
        })
      ]
