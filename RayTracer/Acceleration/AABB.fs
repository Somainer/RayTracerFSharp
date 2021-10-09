namespace RayTracer.Acceleration

open System
open RayTracer

type AABB (minimum, maximum) =
    static member infinity = AABB(Vec3d.only Double.NegativeInfinity, Vec3d.only Double.PositiveInfinity)
    
    member self.isIntersect(ray : Ray inref, tMin, tMax) =
        let mutable tMin = tMin
        let mutable tMax = tMax
        let direction = ray.direction
        let origin = ray.origin
        let inline testOnAxis axis =
            let invDirection = 1.0 / direction.[axis]
            let candidate0 = (minimum.[axis] - origin.[axis]) * invDirection
            let candidate1 = (maximum.[axis] - origin.[axis]) * invDirection
            let t0 = Math.Min(candidate0, candidate1)
            let t1 = Math.Max(candidate0, candidate1)
            tMin <- Math.Max(t0, tMin)
            tMax <- Math.Min(t1, tMax)
            // If tMax <= tMin then no intersection
            tMax > tMin
        
        
        testOnAxis 0
        && testOnAxis 1
        && testOnAxis 2
    
    member this.Minimum = minimum
    member this.Maximum = maximum

    member self.surroundWith (that : AABB) =
        let small = Vec3d.elementwiseMin self.Minimum that.Minimum
        let big = Vec3d.elementwiseMax self.Maximum that.Maximum
        AABB (small, big)
        
    static member surround (this : AABB) that =
        this.surroundWith that
