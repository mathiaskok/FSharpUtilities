namespace FSharpUtilities

open System

module Option =

  let mapObj mapper mx = 
    match mx with
    | None -> None
    | Some x -> 
      let res = mapper x
      match res with
      | null -> None
      | _ -> Some res

  let mapNullable (mapper: 'a -> 'b Nullable) mx =
    match mx with
    | None -> None
    | Some x ->
      let res = mapper x
      match res.HasValue with
      | true -> Some res.Value
      | false -> None

  let apply mf mx = 
    match mf with
    | Some f -> Option.map f mx
    | None -> None

  let (<*>) = apply

  let (>>=) = Option.bind
  
  let bindObj mx binder =
    match mx with
    | None -> None
    | Some x ->
      let res = binder x
      match res with
      | null -> None
      | _ -> Some res

  let (>>=!) = bindObj

  let bindNullable mx (binder: 'a -> 'b Nullable) =
    match mx with
    | None -> None
    | Some x -> 
      let res = binder x
      match res.HasValue with
      | true -> Some res.Value
      | false -> None

  let (>>=?) = bindNullable