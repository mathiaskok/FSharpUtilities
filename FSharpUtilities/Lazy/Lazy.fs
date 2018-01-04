module FSharpUtilities.Lazy

let map mapper (lx: 'a Lazy) = 
  lazy (lx.Value |> mapper)

let apply (lf: ('a -> 'b) Lazy) (lx: 'a Lazy) =
  lazy (lf.Value lx.Value)

let (<*>) = apply

let bind (lx: 'a Lazy) (binder: 'a -> 'b Lazy) =
  lazy ((binder lx.Value).Value)

let (>>=) = bind

let join ll = bind ll id

let map2 mapper (lx: 'a Lazy) (ly: 'b Lazy) =
  map mapper lx <*> ly

let map3 mapper (lx: 'a Lazy) (ly: 'b Lazy) (lz: 'c Lazy) =
  map mapper lx <*> ly <*> lz