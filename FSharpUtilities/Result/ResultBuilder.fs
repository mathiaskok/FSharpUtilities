namespace FSharpUtilities

open FSharpUtilities.Result

module ResultBuilder =

  type ResultBuilder() = 
    member __.Bind(r, binder) = bind r binder

    member __.Return(s) = Success s

    member __.ReturnFrom(r) = r


  let result = ResultBuilder()
