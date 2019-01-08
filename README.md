# FSharpPlus.AspNetCore

[F#+](https://github.com/fsprojects/FSharpPlus) bindings for [asp.net core](https://github.com/fsprojects/FSharpPlus). This is intended to enable development of web api:s leveraging F#+ and asp.net core.

---

## Builds

MacOS/Linux | Windows
--- | ---
[![Travis Badge](https://travis-ci.org/wallymathieu/FSharpPlus.AspNetCore.svg?branch=master)](https://travis-ci.org/wallymathieu/FSharpPlus.AspNetCore) | [![Build status](https://ci.appveyor.com/api/projects/status/4sfk5c67p698aobx/branch/master?svg=true)](https://ci.appveyor.com/project/wallymathieu/fsharpplus-aspnetcore)
[![Build History](https://buildstats.info/travisci/chart/wallymathieu/FSharpPlus.AspNetCore)](https://travis-ci.org/wallymathieu/FSharpPlus.AspNetCore/builds) | [![Build History](https://buildstats.info/appveyor/chart/wallymathieu/fsharpplus-aspnetcore)](https://ci.appveyor.com/project/wallymathieu/fsharpplus-aspnetcore)


## Nuget 

Stable | Prerelease
--- | ---
[![NuGet Badge](https://buildstats.info/nuget/FSharpPlus.AspNetCore)](https://www.nuget.org/packages/FSharpPlus.AspNetCore/) | [![NuGet Badge](https://buildstats.info/nuget/FSharpPlus.AspNetCore?includePreReleases=true)](https://www.nuget.org/packages/FSharpPlus.AspNetCore/)

---

### Building


Make sure the following **requirements** are installed in your system:

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
* [Mono](http://www.mono-project.com/) if you're on Linux or macOS.

```
PM> dotnet build
```

## Getting Started

```
PM> mkdir MySite
PM> dotnet new console -lang f# -o MySite
PM> dotnet add ./MySite package FSharpPlus.AspNetCore
PM> dotnet add ./MySite package FSharpPlus.AspNetCore.Suave
```

You can then add your code in the style of [Suave](https://suave.io/)

```f#
// Learn more about F# at http://fsharp.org

open System

open FSharpPlus

open FSharpPlus.AspNetCore
open FSharpPlus.AspNetCore.Suave
open HttpAdapter
open Successful
open RequestErrors
open Filters
open Writers

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Builder

open Newtonsoft.Json

module Json=
  let inline OK v=
    OK (JsonConvert.SerializeObject v)
    >=> setContentType "application/json; charset=utf-8"
  let inline CREATED v=
    CREATED (JsonConvert.SerializeObject v)
    >=> setContentType "application/json; charset=utf-8"

type Todo = { id:int; text:string }
type IDb =
  abstract member AddTodo: string -> Async<Todo>
  abstract member GetTodo: int ->Async<Todo option>

let myWebPart(db:IDb) =
  let register =
      POST >=> hasFormContentType >=> fun ctx -> monad {
      match ctx.request |> Request.Form.tryGet "text" with
      | Some text ->
        let! todo = lift (db.AddTodo (string text))
        return! Json.CREATED todo ctx
      | None ->
        return! BAD_REQUEST "Could not find form text" ctx
    }
  let getTodo id=
      GET >=> fun ctx -> monad {
        match! lift (db.GetTodo id) with
        | Some todo ->     
          return! Json.CREATED todo ctx
        | None ->
          return! NOT_FOUND "Could not find todo" ctx
    }
  WebPart.choose [ 
                  path "/" >=> (OK "/")
                  path "/todo" >=> register
                  pathScan "/todo/%d" getTodo
                  ]


type CmdArgs =
  { Json : string option }
[<EntryPoint>]
let main argv =
  let buildWebHost args =
    WebHost.CreateDefaultBuilder(args)
      .Configure(fun (app: IApplicationBuilder)->
        let db = { new IDb with
                   member this.AddTodo(text)=failwith "not implemented" 
                   member this.GetTodo(id)=failwith "not implemented" 
                 }
        let webPart = myWebPart db
        app
            |> appMap "/v1/" (Suave.appRun webPart)
            |> ignore
      ).Build()
  buildWebHost(argv).Run()
  0
```

## Alternatives

If you are looking for a composable library for web then [Suave](https://suave.io/) and [Giraffe](https://github.com/giraffe-fsharp/Giraffe) are the ones mostly used. Giraffe should have better performance than this library. Suave has been around the longest (it inspired Giraffe). For a simple web framework you have also [Frank](https://wizardsofsmart.wordpress.com/2019/01/01/yet-another-f-web-framework/).
