namespace FSharpUtilities
module Result =

  type Result<'f, 's> =
    | Success of 's
    | Failure of 'f


  let map mapper rx = 
    match rx with
    | Success s -> Success (mapper s)
    | Failure f -> Failure f

  let apply rf rx =
    match rf with
    | Success f -> map f rx
    | Failure f -> Failure f

  let (<*>) = apply

  let bind rx binder =
    match rx with
    | Success x -> binder x
    | Failure f -> Failure f

  let (>>=) = bind

  let join res = bind res id

  let map2 mapper rx ry = map mapper rx <*> ry

  let map3 mapper rx ry rz = map mapper rx <*> ry <*> rz

  let mapFailure mapper rx =
    match rx with
    | Success s -> Success s
    | Failure f -> Failure (mapper f)

  let combine sMapper fMapper r1 r2 =
    match (r1, r2) with
    | (Success s1, Success s2) -> Success (sMapper s1 s2)
    | (Success _, Failure f) -> Failure f
    | (Failure f, Success _) -> Failure f
    | (Failure f1, Failure f2) -> Failure (fMapper f1 f2)