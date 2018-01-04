module FSharpUtilities.State

type State<'s, 'a> = 's -> 'a * 's

let map mapper (state: State<'s, 'a>) : State<'s, 'b> =
  fun s ->
    let (a, state) = state s
    (mapper a, state)

let bind (state: State<'s, 'a>) (binder: 'a -> State<'s, 'b>): State<'s, 'b> = 
  fun s ->
    let (a, state) = state s
    binder a state

let (>>=) = bind