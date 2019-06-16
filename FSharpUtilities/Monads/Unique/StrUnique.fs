module FSharpUtilities.Monads.StrUnique

type StrUnique = private StrUnique of string

let retrieveStr (StrUnique s) = s

type StrUniqueMonad<'a> = State.State<StrUnique, 'a>

let private exceptLastChar (s:string) = s.Substring(0,s.Length - 1)

let private lastChar (s:string) = s.Chars (s.Length - 1)

let rec nextStr =
  function
  | "" -> "a"
  | s -> 
    let r = exceptLastChar s
    match lastChar s with
    | 'z' -> (nextStr r) + "a"
    | l ->
      let ll = (char)((int)l + 1)
      r + ll.ToString()

let fresh : StrUniqueMonad<StrUnique> =
  fun (StrUnique s) ->
    let nextS = StrUnique (nextStr s)
    (nextS, nextS)

let run (s:StrUniqueMonad<'a>) : 'a =
  s (StrUnique "") |> fst