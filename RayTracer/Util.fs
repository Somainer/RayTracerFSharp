[<AutoOpen>]
module RayTracer.Util

open System
open System.Runtime.CompilerServices

open RayTracer.Numerics

let inline clamp x min max =
    if x < min then min
    else if x > max then max
    else x

let inline replaceNaN c = if isNaN c then 0.0 else c

let gammaCorrection c scale = Math.Sqrt (replaceNaN(c) * scale)

let inline nearZero (x : double) =
    Math.Abs x < Double.Epsilon

let inline implicitly (x : ^t) = ((^t or ^u) : (static member op_Implicit : ^t -> ^u) x)

let inline parse s = (^t : (static member Parse : string -> ^t) s)

type Double with
    member inline this.IsSignNegative = Double.IsNegative this
    member inline this.IsSignPositive = not this.IsSignNegative
