module FSharpUtilities.Option
open System
open FSharpUtilities.General

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

let bindObj x y = flip mapObj x y

let bindNullable x y = flip mapNullable x y

let join ma = Option.bind id ma

type OptionBuilder() =
  inherit StandardBuilderFunctions.BuilderBase()

  member __.Return(x) = Some x
  member __.Bind(o, b) = Option.bind b o

let maybe = OptionBuilder()