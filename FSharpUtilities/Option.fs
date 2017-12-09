namespace FSharpUtilities

open System

module Option =
  
  let (>>=) = Option.bind

  let apply mf mx = 
    match mf with
    | Some f -> Option.map f mx
    | None -> None

  let (<*>) = apply

  let mapObj mapper x =
    let res = mapper x
    match res with
    | null -> None
    | _ -> Some res

  let mapNullable (mapper : 'a -> 'b Nullable) x =
    let res = mapper x
    match res.HasValue with
    | true -> Some res
    | false -> None