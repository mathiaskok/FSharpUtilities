module FSharpUtilities.Writer  

type Writer<'a,'s> = 'a * 's
type WriterMonoid<'s> = 's -> 's -> 's


let ret sZero a : Writer<'a,'s> = (a,sZero)

let map func ((a,s): Writer<'a,'s>) : Writer<'b,'s> = (func a,s)

let apply (monoid: WriterMonoid<'s>) ((func,s1):Writer<'a -> 'b,'s>) ((a,s2):Writer<'a,'s>) : Writer<'b,'s> =
  (func a, monoid s1 s2)

let bind (monoid: WriterMonoid<'s>) ((a,s1): Writer<'a,'s>) (binder: 'a -> Writer<'b,'s>) : Writer<'b,'s> =
  let (b,s2) = binder a
  (b, monoid s1 s2)

let join (monoid: WriterMonoid<'s>) (wwa: Writer<Writer<'a,'s>,'s>) : Writer<'a,'s> =
  bind monoid wwa id

type WriterBuilder<'a>
  (
    zero: 'a,
    monoid: 'a -> 'a -> 'a
  ) =
  inherit StandardBuilderFunctions.BuilderBase()

  member __.Return(a) = ret zero a
  member __.Bind(wa,binder) = bind monoid wa binder

let writer zero monoid = WriterBuilder(zero, monoid)