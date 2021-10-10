[<AutoOpen>]
module RayTracer.Numerics

let inline isNaN (c : ^f) = (^f : (static member IsNaN : ^f -> bool) c)

let inline neg n = -n
