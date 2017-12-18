namespace FSharpUtilities
module Reader =

  type ReaderBuilder() =
    member __.Return(x) = fun _ -> x
    
    member __.ReturnFrom(r) = r

    member __.Bind(reader, binder) =
      fun env ->
        let x = reader env
        binder x env

  let reader = ReaderBuilder()