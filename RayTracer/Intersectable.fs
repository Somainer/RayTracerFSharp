namespace RayTracer.Intersectable

open RayTracer
open RayTracer.Acceleration
open RayTracer.Material
open RayTracer.Util

type IIntersectable =
    abstract member intersect : ray : Ray inref * tMin : double * tMax : double -> ValueOption<HitRecord>
    abstract member boundingBox : time0 : double -> time1 : double -> Option<AABB>
    
type Triangle (v0, v1, v2) =
    let e1 : Vec3d = v1 - v0
    let e2 = v2 - v0
    let cross = e1.cross(e2)
    let normal = cross.normalized
    let area = cross.norm * 0.5

    member self.barycentricCoordinate p =
        let side1 = Vec3d.make (v1.x - v0.x) (v2.x - v0.x) (v0.x - p.x)
        let side2 = Vec3d.make (v1.y - v0.y) (v2.y - v0.y) (v0.y - p.y)
        let sides = side1.cross side2
        let v = sides.x / sides.z
        let w = sides.y / sides.z
        let u = 1.0 - v - w

        if u.IsSignPositive && v.IsSignPositive && w.IsSignPositive then
            ValueSome (struct (u, v, w))
        else ValueNone

    
    interface IIntersectable with
        member this.intersect(ray, tMin, tMax) =
            let nDotDirection = normal.dot ray.direction
            if abs nDotDirection < System.Double.Epsilon then ValueNone
            else 
                let originV0 = ray.origin - v0
                let time = normal.dot(originV0) / ray.direction.dot(normal)
                if time < tMin || time > tMax then ValueNone
                else
                    let point = ray.at time
                    
                    point
                    |> this.barycentricCoordinate
                    |> ValueOption.map (
                        fun struct (_, u, v) ->
                            {
                                HitRecord.t = time
                                u = u; v = v; point = point
                                material = Material.Dummy
                                normal = normal; frontFace = false
                            }
                    )
                
            
        member this.boundingBox _ _ =
            Some (
                AABB(
                    Vec3d.elementwiseMin v0 (Vec3d.elementwiseMin v1 v2),
                    Vec3d.elementwiseMax v0 (Vec3d.elementwiseMax v1 v2)))

type IntersectableList (objects : IIntersectable[]) =
    interface IIntersectable with
        member this.intersect(ray, tMin, tMax) =
            let mutable intersect = ValueNone
            let mutable closest = tMax
            // Manually rewrite to for loop because tailrec function can not capture in refs.
            for object in objects do
                match object.intersect(&ray, tMin, closest) with
                | ValueNone -> ()
                | ValueSome result as res ->
                    closest <- result.t
                    intersect <- res
            intersect
            
        member this.boundingBox time0 time1 =
            let rec loop i result =
                if i = objects.Length then Some result
                else
                    match objects.[i].boundingBox time0 time1 with
                    | None -> None
                    | Some box ->
                        let newBox = AABB.surround result box
                        loop (i + 1) newBox
            
            if objects.Length = 0 then None else loop 0 AABB.infinity
            
            
type OverridenMaterial (object : IIntersectable, material) =
    interface IIntersectable with
        member this.boundingBox time0 time1 = object.boundingBox time0 time1
        member this.intersect(ray, tMin, tMax) =
            let intersection = object.intersect(&ray, tMin, tMax)
            intersection
            |> ValueOption.map (HitRecord.withMaterial material)
module OverridenMaterial =
    type IIntersectable with
        member self.withMaterial material = OverridenMaterial(self, material)
