namespace FSharpUtilities
module State =

  type StateBuilder() =
    member __.Return(x) = fun s -> (x, s)
    
    member __.ReturnFrom(s) = s

    member __.Binder(state, binder) =
      fun s ->
        let (value, state) = state s
        let (value', state') = binder value state
        (value', state')
