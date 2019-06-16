module FSharpUtilities.Dictionary.ReadOnlyDictionary

open System.Collections.Generic

let private toKvpFun p (kvp: KeyValuePair<'k, 'v>) = p kvp.Key kvp.Value

let private toKvpFolder f =
  fun (s: 's) (kvp: KeyValuePair<'k, 'v>) -> f s kvp.Key kvp.Value

let private kvpToTuple (kvp: KeyValuePair<'a, 'b>) = (kvp.Key, kvp.Value)

let raiseKeyEx _ = raise (KeyNotFoundException())

let count (dict: IReadOnlyDictionary<'a, 'b>) = dict.Count

let containsKey (dict: IReadOnlyDictionary<'a, 'b>) = dict.ContainsKey

let exits predicate (dict: IReadOnlyDictionary<'a, 'b>) = 
  Seq.exists (toKvpFun predicate) dict

let find a (dict: IReadOnlyDictionary<'a, 'b>) = dict.[a]

let tryFind a (dict: IReadOnlyDictionary<'a, 'b>) =
  match dict.TryGetValue a with
  | (true, x) -> Some x
  | (false, _) -> None

let private findKeyI cont mapper predicate (dict: IReadOnlyDictionary<'k, 'v>) =
  match Seq.tryFind (toKvpFun predicate) dict with
  | Some k -> mapper k.Key
  | None -> cont()

let findKey predicate dict =
  findKeyI raiseKeyEx id predicate dict 

let tryFindKey predicate dict =
  findKeyI (fun _ -> None) Some predicate dict

let fold (folder: 'a -> 'b -> 'c -> 'a) state (dict: IReadOnlyDictionary<'b, 'c>) =
  Seq.fold (toKvpFolder folder) state dict

let forall predicate (dict: IReadOnlyDictionary<'a, 'b>) =
  Seq.forall (toKvpFun predicate) dict

let isEmpty: IReadOnlyDictionary<'a, 'b> -> bool = Seq.isEmpty

let iter action (dict: IReadOnlyDictionary<'a, 'b>) =
  Seq.iter (toKvpFun action) dict

let toSeq (dict: IReadOnlyDictionary<'a, 'b>) =
  Seq.map kvpToTuple dict

let toList (dict: IReadOnlyDictionary<'a, 'b>) =
  List.ofSeq (toSeq dict)

let toArray (dict: IReadOnlyDictionary<'a, 'b>) =
  let arr: ('a * 'b) array = Array.zeroCreate dict.Count
  let mutable i = 0
  let action kvp = 
    arr.[i] <- kvpToTuple kvp
    i <- i+1

  Seq.iter action dict
  arr

let private pickI cont mapper chooser (dict: IReadOnlyDictionary<'k, 'v>) =
  let valid = 
    dict
    |> Seq.map (toKvpFun chooser)
    |> Seq.filter Option.isSome

  match Seq.tryHead valid with
  | Some (Some x) -> mapper x
  | _ -> cont()

let pick chooser dict =
  pickI raiseKeyEx id chooser dict

let tryPick chooser dict = 
  pickI (fun _ -> None) Some chooser dict