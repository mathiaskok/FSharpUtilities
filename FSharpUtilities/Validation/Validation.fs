module FSharpUtilities.Validation

open System
open FSharpUtilities.General

type Validation<'s, 'f> = Result<'s, 'f seq>

let private ret : ('s -> Validation<'s,'f>) = Result.Ok

let private returnFrom : (Validation<'s, 'f> -> Validation<'s, 'f>) = id

let map (func: 's1 -> 's2) (va: Validation<'s1,'f>) : Validation<'s2,'f> = 
  match va with
  | Ok s -> Ok (func s)
  | Error f -> Error f

let mapErrors (func: 'f1 -> 'f2) (va: Validation<'s,'f1>) : Validation<'s,'f2> = 
  match va with
  | Ok s -> Ok s
  | Error f -> Error (Seq.map func f)

let apply (vFunc: Validation<'s1 -> 's2, 'f>) (va: Validation<'s1,'f>) : (Validation<'s2,'f>) =
  match (vFunc, va) with
  | (Ok func, Ok s) -> Ok (func s)
  | (Error err1, Error err2) -> Error (Seq.concat [|err1;err2|])
  | (Error err, _) -> Error err
  | (_, Error err) -> Error err

let (<*>) = apply

let bind (va: Validation<'s1,'f>) (binder: 's1 -> Validation<'s2,'f>) : Validation<'s2,'f> =
  match va with
  | Ok s -> binder s
  | Error f -> Error f

let (>>=) = bind

let liftValidation1 func vx = 
  map func vx

let liftValidation2 func vx vy = 
  map func vx <*> vy

let liftValidation3 func vx vy vz = 
  map func vx <*> vy <*> vz

let liftValidation4 func vx vy vz vw = 
  map func vx <*> vy <*> vz <*> vw

let applyValidationFunc1 func = 
  liftValidation1 (fromFunc1 func)

let applyValidationFunc2 func vy = 
  liftValidation2 (fromFunc2 func)

let applyValidationFunc3 func = 
  liftValidation3 (fromFunc3 func)

let applyValidationFunc4 func =
  liftValidation4 (fromFunc4 func)

let private tryWith (body: unit -> Validation<'s,'f>) (handler: exn -> Validation<'s,'f>) : Validation<'s,'f> =
  try
    returnFrom(body())
  with
    | e -> handler e  

let private tryFinally (body: unit -> Validation<'s,'f>) (compensation: unit -> unit) =
  try
    returnFrom(body())
  finally
    compensation()  

let private using (disposable:#System.IDisposable) (body) =
  let body = fun () -> body disposable
  tryFinally body (fun () -> 
    match disposable with
    | null -> ()
    | disp -> disp.Dispose())


type ValidationBuilder() = 
  member this.Return(x) = ret x
  member this.ReturnFrom(vx) = returnFrom
  member this.Bind(x,binder) = bind x binder
  member this.Delay(func) = func()
  member this.TryWith(body,hanlder) = tryWith body hanlder
  member this.TryFinally(body,compensation) = tryFinally body compensation
  member this.Using(disp,body) = using disp body


let validation = ValidationBuilder()