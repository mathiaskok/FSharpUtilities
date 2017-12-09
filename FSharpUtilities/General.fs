namespace FSharpUtilities
module General =
  
  let uncurry2 f (x, y) = f x y
  let uncurry3 f (x, y, z) = f x y z
  let uncurry4 f (x, y, z, t) = f x y z t
  let uncurry5 f (x, y, z, t, u) = f x y z t u

  let curry2 f x y = f (x, y)
  let curry3 f x y z = f (x, y, z)
  let curry4 f x y z t = f (x, y, z, t)
  let curry5 f x y z t u = f (x, y, z, t, u)