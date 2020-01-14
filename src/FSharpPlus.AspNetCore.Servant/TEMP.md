    WebPart.choose [ path "/" >=> (OK "/")
                     path "/notes" >=> register
                     pathScan "/notes/%d" getNote
                     pathScan "/notes/%d" updateNote
                     path "/notes" >=> overview ]



let myApp =
  choose [
    GET >=> choose
      [ path "/hello" >=> OK "Hello GET" ; path "/goodbye" >=> OK "Good bye GET" ];
    POST >=> choose
      [ path "/hello" >=> OK "Hello POST" ; path "/goodbye" >=> OK "Good bye POST" ];
    DELETE >=> choose
      [ path "/hello" >=> OK "Hello DELETE" ; path "/goodbye" >=> OK "Good bye DELETE" ];
    PUT >=> choose
      [ path "/hello" >=> OK "Hello PUT" ; path "/goodbye" >=> OK "Good bye PUT" ];
  ]

 type API =
        "a" :> "d" :> Get '[JSON] NoContent
   :<|> "b" :> Capture "x" Int :> Get '[JSON] Bool
   :<|> "c" :> Put '[JSON] Bool
   :<|> "a" :> "e" :> Get '[JSON] Int
   :<|> "b" :> Capture "x" Int :> Put '[JSON] Bool
   :<|> Raw

->


 /
 ├─ a/
 │  ├─ d/
 │  │  └─•
 │  └─ e/
 │     └─•
 ├─ b/
 │  └─ <capture>/
 │     ├─•
 │     ┆
 │     └─•
 ├─ c/
 │  └─•
 ┆
 └─ <raw>

 Explanation of symbols:

 [@├@] Normal lines reflect static branching via a table.

 [@a/@] Nodes reflect static path components.

 [@─•@] Leaves reflect endpoints.

 [@\<capture\>/@] This is a delayed capture of a path component.

 [@\<raw\>@] This is a part of the API we do not know anything about.

 [@┆@] Dashed lines suggest a dynamic choice between the part above
 and below. If there is a success for fatal failure in the first part,
 that one takes precedence. If both parts fail, the \"better\" error
 code will be returned.



data Router' env a =
    StaticRouter  (Map Text (Router' env a)) [env -> a]
      -- ^ the map contains routers for subpaths (first path component used
      --   for lookup and removed afterwards), the list contains handlers
      --   for the empty path, to be tried in order
  | CaptureRouter (Router' (Text, env) a)
      -- ^ first path component is passed to the child router in its
      --   environment and removed afterwards
  | CaptureAllRouter (Router' ([Text], env) a)
      -- ^ all path components are passed to the child router in its
      --   environment and are removed afterwards
  | RawRouter     (env -> a)
      -- ^ to be used for routes we do not know anything about
  | Choice        (Router' env a) (Router' env a)
      -- ^ left-biased choice between two routers
