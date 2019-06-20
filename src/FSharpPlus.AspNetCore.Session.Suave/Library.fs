module FSharpPlus.AspNetCore.Session.Suave
open FSharpPlus
open FSharpPlus.Data
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Session
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open System.Net
open System.Text
open System
open System.Text.RegularExpressions
open FSharp.Scanf.Optimized
type StateStore(v:byte[])=class
  end
module Http=
  module Context=
    let state (ctx : HttpContext) =
      match ctx.Session.TryGetValue "mc-st" with
      | true, v-> StateStore v |> Some
      | _ -> None
