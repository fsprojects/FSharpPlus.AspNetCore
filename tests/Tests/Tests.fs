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
    abstract member getUserNotes: int -> Async<NoteList>
    abstract member addUserNote: int -> string -> Async<Note>

let webApp (db: IDb) =
    let overview =
        GET >=> (authenticated <| fun ctx userId ->
            monad {
              let! res = lift (db.getUserNotes userId)
              let ovm = toJson res |> string
              return! OK ovm ctx
            })
    let register =
        POST >=> (authenticated <| fun ctx userId ->
            monad {
              match ctx.request |> Request.Form.tryGet "text" with
              | Some text ->
                  let! newNote = lift (db.addUserNote userId (string text))
                  let rvm = toJson newNote |> string
                  return! OK rvm ctx
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

module Program=
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
[<Tests>]
let tests =
  testList "samples" [
    testCase "Say nothing" <| fun _ ->
      //let subject = Say.nothing ()
      Expect.equal "subject" "()" "Not an absolute unit"
    testCase "Say hello all" <| fun _ ->
      //let subject = Say.hello "all"
      Expect.equal "subject" "Hello all" "You didn't say hello"
  ]
