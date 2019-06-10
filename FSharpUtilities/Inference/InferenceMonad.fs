module FSharpUtilities.InferenceMonad

type InferenceMonad<'a,'err> = StrUnique.StrUniqueMonad<Result<'a, 'err>>

let retRes x : InferenceMonad<'a,'err> = State.ret x

let ret x : InferenceMonad<'a,'err> = retRes (Ok x)

let map (mapper: 'a -> 'b) (inf:InferenceMonad<'a,'err>) : InferenceMonad<'b,'err> = 
  State.map (Result.map mapper) inf

let apply (func:InferenceMonad<'a -> 'b,'err>) (infA:InferenceMonad<'a,'err>) : InferenceMonad<'b,'err> =
  fun s ->
    let (f,s) = func s
    let (a,s) = infA s
    (Result.apply f a,s)

let bind (inf:InferenceMonad<'a,'err>) (binder:'a -> InferenceMonad<'b,'err>) : InferenceMonad<'b,'err> =
  fun s -> 
    match inf s with
    | (Error err,s) -> (Error err,s)
    | (Ok a,s) -> binder a s

let join (a:InferenceMonad<InferenceMonad<'a,'err>,'err>) : InferenceMonad<'a,'err> = 
  bind a id

type InferenceBuilder() = 
  member this.Return(x) = ret x
  member this.ReturnFrom(vx) = StandardBuilderFunctions.returnFrom vx
  member this.Bind(x,binder) = bind x binder
  member this.Delay(func) = func()
  member this.TryWith(body,hanlder) = StandardBuilderFunctions.tryWith body hanlder
  member this.TryFinally(body,compensation) = StandardBuilderFunctions.tryFinally body compensation
  member this.Using(disp,body) = StandardBuilderFunctions.using disp body

let inference = InferenceBuilder()