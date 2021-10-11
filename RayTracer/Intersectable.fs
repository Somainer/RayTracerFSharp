namespace rec RayTracer.Intersectable

open RayTracer
open RayTracer.Acceleration
open RayTracer.Material
open RayTracer.Util

type IIntersectable =
    abstract member intersect : ray : Ray inref * tMin : double * tMax : double -> ValueOption<HitRecord>
    abstract member boundingBox : time0 : double -> time1 : double -> Option<AABB>
    
type Triangle (v0, v1, v2) =
    let e1 : Vec3d = v1 - v0
    let e2 : Vec3d = v2 - v0
    let cross = e1.cross(e2)
    let normal = cross.normalized
    let area = cross.norm * 0.5
    
    member self.v0 = v0
    member self.v1 = v1
    member self.v2 = v2

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
//        member this.intersect(ray, tMin, tMax) =
//            let nDotDirection = normal.dot ray.direction.normalized
//            if abs nDotDirection < System.Double.Epsilon then ValueNone
//            else 
//                let originV0 = ray.origin - v0
//                let time = normal.dot(originV0) / nDotDirection
//                if time < tMin || time > tMax then ValueNone
//                else
//                    let point = ray.at time
//                    
//                    point
//                    |> this.barycentricCoordinate
//                    |> ValueOption.map (
//                        fun struct (_, u, v) ->
//                            {
//                                HitRecord.t = time
//                                u = u; v = v; point = point
//                                material = Material.Dummy
//                                normal = normal; frontFace = false
//                            }
//                    )
        member self.intersect(ray, tMin, tMax) =
            match Triangle.solveTriangle self ray.origin ray.direction.normalized tMin tMax with
            | ValueNone -> ValueNone
            | ValueSome struct (t, _, u, v) ->
               HitRecord.newWithFaceNormal t (ray.at t) u v normal Material.Dummy &ray
               |> ValueSome
                
            
        member this.boundingBox _ _ =
            Some (
                AABB(
                    Vec3d.elementwiseMin v0 (Vec3d.elementwiseMin v1 v2),
                    Vec3d.elementwiseMax v0 (Vec3d.elementwiseMax v1 v2)))

module Triangle =
    let solveTriangle (triangle : Triangle) origin direction tMin tMax =
        let p1 = triangle.v0
        let p2 = triangle.v1
        let p3 = triangle.v2
        let a = p1.x - p2.x
        let b = p1.y - p2.y
        let c = p1.z - p2.z
        let d = p1.x - p3.x
        let e = p1.y - p3.y
        let f = p1.z - p3.z
        let g = -direction.x
        let h = -direction.y
        let i = -direction.z
        let j = p1.x - origin.x
        let k = p1.y - origin.y
        let l = p1.z - origin.z
        let ei_hf = e * i - h * f
        let gf_di = g * f - d * i
        let dh_eg = d * h - e * g
        let ak_jb = a * k - j * b
        let jc_al = j * c - a * l
        let bl_kc = b * l - k * c
        let M = a * ei_hf + b * gf_di + c * dh_eg
        let t = (f * ak_jb + e * jc_al + d * bl_kc) / M
        let beta = (j * ei_hf + k * gf_di + l * dh_eg) / M
        let gamma = (i * ak_jb + h * jc_al + g * bl_kc) / M
        let alpha = 1.0 - beta - gamma
        
        if t > tMin && t < tMax
            && alpha.IsSignPositive
            && beta.IsSignPositive
            && gamma.IsSignPositive then
            ValueSome struct(t, alpha, beta, gamma)
        else ValueNone
       

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

[<AutoOpen>]
module OverridenMaterial =
    type IIntersectable with
        member self.withMaterial material = OverridenMaterial(self, material)
