namespace FSharpUtilities

open FSharpUtilities.Result

module ResultBuilder =

  type ResultBuilder() = 
    member __.Bind(r, binder) = bind r binder

    member __.Return(s) = Ok s

    member __.ReturnFrom(r) = r


  let result = ResultBuilder()

  type YieldResultBuilder<'s, 'f>
    (
      sCombiner: 's -> 's -> 's, 
      fCombiner: 'f -> 'f -> 'f
    ) =

    member __.Bind(r, binder) = bind r binder

    member __.YieldFrom(r) = r

    member __.Delay(f) = f()


  let yieldResult sCombiner fCombiner = YieldResultBuilder(sCombiner, fCombiner)
