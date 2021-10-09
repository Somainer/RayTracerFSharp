module RayTracer.Render

open System
open RayTracer.Intersectable

let rec traceRay (ray : Ray inref) (world : #IIntersectable) (background : Color3d) maxDepth : Color3d =
    if maxDepth <= 0 then Vec3d.zero
    else
        match world.intersect(&ray, 0.001, Double.PositiveInfinity) with
        | ValueNone -> background
        | ValueSome intersect ->
            let material = intersect.material
            let emitted = material.emitted intersect.u intersect.v intersect.point
            match material.scatter(&ray, &intersect) with
            | ValueNone -> emitted
            | ValueSome struct (color, scattered) ->
                let scatteredColor = traceRay &scattered world background (maxDepth - 1)
                emitted + color * scatteredColor
