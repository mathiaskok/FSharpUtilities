module FSharpUtilities.Monads.StandardBuilderFunctions

let returnFrom = id

let tryWith (body: unit -> 'a) (handler: exn -> 'a) : 'a =
  try
    returnFrom(body())
  with
    | e -> handler e  

let tryFinally (body: unit -> 'a) (compensation: unit -> unit) =
  try
    returnFrom(body())
  finally
    compensation() 

let using (disposable:#System.IDisposable) body =
  let body = fun () -> body disposable
  tryFinally body (fun () -> 
    match disposable with
    | null -> ()
    | disp -> disp.Dispose())

[<AbstractClass>]
type BuilderBase() =
  member __.ReturnFrom(r) = returnFrom r
  member __.Delay(f) = f()
  member __.TryWith(body,handler) = tryWith body handler
  member __.TryFinally(body,compensation) = tryFinally body compensation
  member __.Using(disp,body) = using disp body