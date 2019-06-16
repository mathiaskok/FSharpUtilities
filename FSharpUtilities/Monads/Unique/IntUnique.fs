module FSharpUtilities.Monads.IntUnique

type IntUnique = private IntUnique of int

let retrieveInt (IntUnique i) = i

type IntUniqueMonad<'a> = State.State<IntUnique, 'a>

let fresh : IntUniqueMonad<IntUnique> =
  fun (IntUnique s) ->
    let nextS = IntUnique (s + 1)
    (nextS, nextS)

let run (i:IntUniqueMonad<'a>) : 'a =
  i (IntUnique -1) |> fst