module FSharpUtilities.StandardBuilderFunctions

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