module FSharpUtilities.General
  
open System

let uncurry2 f (x, y) = f x y
let uncurry3 f (x, y, z) = f x y z
let uncurry4 f (x, y, z, t) = f x y z t
let uncurry5 f (x, y, z, t, u) = f x y z t u

let curry2 f x y = f (x, y)
let curry3 f x y z = f (x, y, z)
let curry4 f x y z t = f (x, y, z, t)
let curry5 f x y z t u = f (x, y, z, t, u)

let flip f x y = f y x

let toAction0 f = Action(f)
let toAction1 f = Action<'a>(f)
let toFunc0 f = Func<'a>(f)
let toFunc1 f = Func<'a,'b>(f)
let toFunc2 f = Func<'a,'b,'c>(f)
let toFunc3 f = Func<'a,'b,'c,'d>(f)

let fromAction0 (a: Action) = a.Invoke
let fromAction1 (a: Action<'a>) = a.Invoke
let fromFunc0 (f: Func<'a>) = f.Invoke
let fromFunc1 (f: Func<'a,'b>) = f.Invoke
let fromFunc2 (f: Func<'a,'b,'c>) = curry2 f.Invoke
let fromFunc3 (f: Func<'a,'b,'c,'d>) = curry3 f.Invoke