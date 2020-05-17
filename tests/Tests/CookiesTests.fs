module CookiesTests

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
let sessionState f =
  context( fun r ->
    match Http.Context.state r with
    | None ->  RequestErrors.BAD_REQUEST "damn it"
    | Some store -> f store )

let webpart () =
  WebPart.choose [ path "/" >=> (OK "/")
                   path "/session"
                    >=> statefulForSession // Session.State.CookieStateStore
                    >=> context (fun x ->
                      match HttpContext.state x with
                      | None ->
                        // restarted server without keeping the key; set key manually?
                        let msg = "Server Key, Cookie Serialiser reset, or Cookie Data Corrupt, "
                                  + "if you refresh the browser page, you'll have gotten a new cookie."
                        OK msg

                      | Some store ->
                        match store.get "counter" with
                        | Some y ->
                          store.set "counter" (y + 1)
                          >=> OK (sprintf "Hello %d time(s)" (y + 1) )
                        | None ->
                          store.set "counter" 1
                          >=> OK "First time")
                   ]
