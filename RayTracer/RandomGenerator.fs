module RayTracer.RandomGenerator

open System

let private rand = Random()

let nextDouble () = rand.NextDouble()

let randomRange min max =
    let diff = max - min
    if diff < Double.Epsilon then min
    else
        nextDouble() * diff + min
        
let randomInt min max = rand.Next(min, max)
