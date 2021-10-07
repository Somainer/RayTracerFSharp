namespace rec RayTracer

open System

[<Struct>]
type Angle =
    | Degree of degree: double
    | Radian of radian: double
    
    member this.deg =
        match this with
        | Degree deg -> deg
        | Radian rad -> Angle.radToDeg rad
    
    member this.rad =
        match this with
        | Degree deg -> Angle.degToRad deg
        | Radian rad -> rad

module Angle =
    let degToRad deg = deg * Math.PI / 180.0
    let radToDeg rad = rad * 180.0 / Math.PI
