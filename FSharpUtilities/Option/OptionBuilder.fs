module FSharpUtilities.OptionBuilder

type OptionBuilder() =
  member __.Return(x) = Some x
  member __.ReturnWith(o) = o
  member __.Bind(o, b) = Option.bind b o

let maybe = OptionBuilder()