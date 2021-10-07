module RayTracer.Util

open System
open System.Runtime.CompilerServices

let inline clamp x min max =
    if x < min then min
    else if x > max then max
    else x

let inline replaceNaN c = if Double.IsNaN c then 0.0 else c

let gammaCorrection c scale = Math.Sqrt (replaceNaN(c) * scale)

let inline nearZero (x : double) =
    Math.Abs x < Double.Epsilon

type Double with
    member inline this.IsSignNegative = Double.IsNegative this
    member inline this.IsSignPositive = not this.IsSignNegative
