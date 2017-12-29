module FSharpUtilities.Option

open FSharpUtilities.General
open System

let mapObj mapper mx = 
  match mx with
  | None -> None
  | Some x -> mapper x |> Option.ofObj

let mapNullable (mapper: 'a -> 'b Nullable) mx =
  match mx with
  | None -> None
  | Some x -> mapper x |> Option.ofNullable

let apply mf mx = 
  match mf with
  | Some f -> Option.map f mx
  | None -> None

let (<*>) = apply

let (>>=) o f = flip Option.bind o f

let bindObj x y = flip mapObj x y

let (>>=!) = bindObj

let bindNullable x y = flip mapNullable x y

let (>>=?) = bindNullable