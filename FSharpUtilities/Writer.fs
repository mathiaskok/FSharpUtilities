namespace FSharpUtilities
module Writer =
  
  type WriterBuilder<'a>
    (
      zero: 'a,
      monoid: 'a -> 'a -> 'a
    ) =

    member __.Return(a) = (a, zero)

    member __.ReturnFrom(w) = w

    member __.Bind((a, acc), writer) =
      let (a', acc') = writer a
      (a', monoid acc acc')

  let writer zero monoid = WriterBuilder(zero, monoid)