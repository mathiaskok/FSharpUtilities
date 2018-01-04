module FSharpUtilities.State

type State<'s, 'a> = 's -> 'a * 's

type StateBuilder() =
  member __.Return(x) : State<'s, 'a> = fun s -> (x, s)
  
  member __.ReturnFrom(s) = s

  member __.Bind(state: State<'s, 'a>, binder: 'a -> State<'s, 'b>) : State<'s, 'b> =
    fun s ->
      let (value, state) = state s
      binder value state

let state = StateBuilder()