module FSharpUtilities.Dictionary.ReadOnly

open System.Collections.Generic

let b = Map.forall

let c = Seq.fold

let private toKvpPredicate p (kvp: KeyValuePair<'k, 'v>) = p kvp.Key kvp.Value

let private toKvpFolder f =
  fun (s: 's) (kvp: KeyValuePair<'k, 'v>) -> f s kvp.Key kvp.Value

let count (dict: IReadOnlyDictionary<'a, 'b>) = dict.Count

let containsKey (dict: IReadOnlyDictionary<'a, 'b>) = dict.ContainsKey

let exits predicate (dict: IReadOnlyDictionary<'a, 'b>) = 
  Seq.exists (toKvpPredicate predicate) dict

//filter

let find a (dict: IReadOnlyDictionary<'a, 'b>) = dict.[a]

let findKey predicate (dict: IReadOnlyDictionary<'a, 'b>) =
  match Seq.tryFind (toKvpPredicate predicate) dict with
  | Some k -> k.Key
  | None -> raise (KeyNotFoundException())

let fold (folder: 'a -> 'b -> 'c -> 'a) state (dict: IReadOnlyDictionary<'b, 'c>) =
  Seq.fold (toKvpFolder folder) state dict

//foldback

let forall predicate (dict: IReadOnlyDictionary<'a, 'b>) =
  Seq.forall (toKvpPredicate predicate) dict

let tryFind a (dict: IReadOnlyDictionary<'a, 'b>) =
  match dict.TryGetValue a with
  | (true, x) -> Some x
  | (false, _) -> None