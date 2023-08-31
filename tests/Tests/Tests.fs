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
open Notes

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
