module FSharpUtilities.Monads.Reader

type Reader<'s,'a> = 's -> 'a

let ret a : Reader<'s,'a> = fun _ -> a

let map func (ra: Reader<'s,'a>) : Reader<'s,'b> =
  fun s ->
    let a = ra s
    func a
    
let apply (rfunc: Reader<'s,'a -> 'b>) (ra: Reader<'s,'a>) : Reader<'s,'b> =
  fun s ->
    let func = rfunc s
    let a = ra s
    func a

let bind (ra: Reader<'s,'a>) (binder: 'a -> Reader<'s,'b>) : Reader<'s,'b> =
  fun s ->
    let a = ra s
    binder a s

let join (rra: Reader<'s,Reader<'s,'a>>) : Reader<'s,'a> =
  fun s ->
    let ra = rra s
    ra s

type ReaderBuilder() =
  inherit StandardBuilderFunctions.BuilderBase()

  member __.Return(x) = ret x
  member __.Bind(s, binder) = bind s binder

let reader = ReaderBuilder()