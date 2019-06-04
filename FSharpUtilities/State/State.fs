module FSharpUtilities.State

type State<'s, 'a> = 's -> 'a * 's

let ret a : State<'s,'a> = fun s -> (a,s)

let map mapper (state: State<'s, 'a>) : State<'s, 'b> =
  fun s ->
    let (a, state) = state s
    (mapper a, state)

let apply (sf: State<'s, 'a -> 'b>) (sx: State<'s, 'a>) : State<'s, 'b> =
  fun s ->
    let (ab, sab) = sf s
    let (a, sa) = sx sab
    (ab a, sa)

let (<*>) = apply

let bind (state: State<'s, 'a>) (binder: 'a -> State<'s, 'b>): State<'s, 'b> = 
  fun s ->
    let (a, state) = state s
    binder a state

let (>>=) = bind

let join (state: State<'s, State<'s, 'a>>) : State<'s, 'a> =
  fun s ->
    let (monad, state) = state s
    monad state