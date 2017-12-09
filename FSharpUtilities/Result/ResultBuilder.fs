namespace FSharpUtilities

open FSharpUtilities.Result

module ResultBuilder =

  type ResultBuilder() = 
    member __.Bind(r, binder) = bind r binder

    member __.Return(s) = Success s

    member __.ReturnFrom(r) = r


  let result = ResultBuilder()

  type YieldResultBuilder<'s, 'f>
    (
      sCombiner: 's -> 's -> 's, 
      fCombiner: 'f -> 'f -> 'f
    ) =

    member __.Bind(r, binder) = bind r binder

    member __.Yield(s) = Success s

    member __.YieldFrom(r) = r

    member __.Combine(r1, r2) = combine sCombiner fCombiner r1 r2

    member __.Delay(f) = f()


  let yieldResult sCombiner fCombiner = YieldResultBuilder(sCombiner, fCombiner)
