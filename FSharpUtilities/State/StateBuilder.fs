module FSharpUtilities.StateBuilder

open FSharpUtilities.State

type StateBuilder() =
  member __.Return(x) : State<'s, 'a> = fun s -> (x, s)
  
  member __.ReturnFrom(s) = s

  member __.Bind(state, binder) = bind state binder

let state = StateBuilder()