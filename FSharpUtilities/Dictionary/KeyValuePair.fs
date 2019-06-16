module FSharpUtilities.Dictionary.KeyValuePair
open System.Collections.Generic

type private KVP<'k,'v> = KeyValuePair<'k,'v>

let (|KVPv|) (kvp: KVP<'k,'v>) = struct (kvp.Key,kvp.Value)
let (|KVP|) (kvp: KVP<'k,'v>) = (kvp.Key,kvp.Value)

let toTuple (KVP t) = t
let toValTuple (KVPv t) = t

let pipeKVP (kvp: KVP<'k,'v>) func = func kvp.Key kvp.Value

let fromTuple (k,v) = KeyValuePair(k,v)
let fromValTuple struct(k,v) = KeyValuePair(k,v)