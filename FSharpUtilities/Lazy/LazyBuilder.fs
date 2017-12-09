namespace FSharpUtilities

open FSharpUtilities.Lazy

module LazyBuilder =
  
  type LazyBuilder() =
    member __.Return(v) = lazy (v)

    member __.ReturnFrom(l) = l

    member __.Bind(l: 'a Lazy, binder: 'a -> 'b Lazy) = bind l binder

  let lazyBuilder = LazyBuilder()