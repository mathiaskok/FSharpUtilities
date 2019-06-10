module FSharpUtilities.Validation

open FSharpUtilities.General

type Validation<'s, 'f> = Result<'s, 'f seq>

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

type ValidationBuilder() = 
  member this.Return(x) = Ok x
  member this.ReturnFrom(vx) = StandardBuilderFunctions.returnFrom vx
  member this.Bind(x,binder) = bind x binder
  member this.Delay(func) = func()
  member this.TryWith(body,hanlder) = StandardBuilderFunctions.tryWith body hanlder
  member this.TryFinally(body,compensation) = StandardBuilderFunctions.tryFinally body compensation
  member this.Using(disp,body) = StandardBuilderFunctions.using disp body


let validation = ValidationBuilder()