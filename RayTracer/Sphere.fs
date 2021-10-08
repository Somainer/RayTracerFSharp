namespace rec RayTracer

open System
open RayTracer.Acceleration
open RayTracer.Intersectable
open RayTracer.Material
open RayTracer.Util

type Sphere (origin, radius, material : Material) =
    member self.sphereUV p =
        let theta = acos -p.y
        let phi : double = (atan2 -p.z p.x) + Math.PI
        
        struct (phi / Math.Tau, theta / Math.PI)
    
    interface IIntersectable with
        member self.boundingBox _ _ =
            let radiusVec = Vec3d.only radius
            Some (AABB(origin - radiusVec, origin + radiusVec))
        member self.intersect(ray, tMin, tMax) =
            Sphere.solveSphereEquation &ray origin radius tMin tMax
            |> ValueOption.map
                (fun struct (root, point, outwardNormal) ->
                    let struct (u, v) = self.sphereUV outwardNormal
                    {
                        HitRecord.t = root
                        point = point; u = u; v = v
                        normal = outwardNormal
                        material = material
                    })
    

module Sphere =
    let solveSphereEquation (ray : Ray inref) center radius tMin tMax =
        let oc = ray.origin - center
        let a = ray.direction.normSquared
        let halfB = oc.dot(ray.direction)
        let c = oc.normSquared - radius * radius
        
        let discriminant = halfB * halfB - a * c
        if discriminant.IsSignNegative then ValueNone
        else
            let sqrtD = sqrt discriminant
            let inline tryRoot (ray : Ray) root =
                if root < tMin || root > tMax then ValueNone
                else
                    let point = ray.at root
                    ValueSome struct(root, point, (point - center) / radius)
                    
            tryRoot ray ((-halfB - sqrtD) / a)
            |> ValueOption.orElse (tryRoot ray ((-halfB + sqrtD) / a))
