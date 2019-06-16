module FSharpUtilities.Monads.State

type State<'s,'a> = 's -> 'a * 's

let ret a : State<'s,'a> = fun s -> (a,s)

let map mapper (state: State<'s,'a>) : State<'s,'b> =
  fun s ->
    let (a, state) = state s
    (mapper a, state)

let apply (sf: State<'s,'a -> 'b>) (sx: State<'s,'a>) : State<'s,'b> =
  fun s ->
    let (ab, sab) = sf s
    let (a, sa) = sx sab
    (ab a, sa)

let bind (state: State<'s,'a>) (binder: 'a -> State<'s,'b>): State<'s,'b> = 
  fun s ->
    let (a, state) = state s
    binder a state

let join (ssa: State<'s, State<'s,'a>>) : State<'s,'a> =
  bind ssa id

type StateBuilder() =
  inherit StandardBuilderFunctions.BuilderBase()

  member __.Return(x) = ret x
  member __.Bind(state, binder) = bind state binder

let state = StateBuilder()