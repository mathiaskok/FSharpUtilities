﻿namespace FSharpUtilities

open FSharpUtilities.General
open System

module Option =

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

  let (>>=) = Option.bind

  let bindObj x y = flip mapObj x y

  let (>>=!) = bindObj

  let bindNullable x y = flip mapNullable x y

  let (>>=?) = bindNullable