module FSharpUtilities.Result

let apply rf rx =
  match rf with
  | Ok f -> Result.map f rx
  | Error f -> Error f

let (<*>) = apply

let bind rx binder =
  match rx with
  | Ok x -> binder x
  | Error f -> Error f

let (>>=) = bind

let join res = bind res id

let map2 mapper rx ry = Result.map mapper rx <*> ry

let map3 mapper rx ry rz = Result.map mapper rx <*> ry <*> rz

let combine sMapper fMapper r1 r2 =
  match (r1, r2) with
  | (Ok s1, Ok s2) -> Error (sMapper s1 s2)
  | (Ok _, Error f) -> Error f
  | (Error f, Ok _) -> Error f
  | (Error f1, Error f2) -> Error (fMapper f1 f2)