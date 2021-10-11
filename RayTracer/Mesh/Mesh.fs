namespace RayTracer.Mesh

open System.IO
open System.Linq
open RayTracer.Acceleration.BVH
open RayTracer.Intersectable

type Mesh = BVH

module Mesh =
    let load (path : string) tMin tMax =
        use reader = new StreamReader(path)
        let mesh = WavefrontMesh.LoadFromObj reader
        let triangles = seq {
            for v in mesh.faces do
                match v.ToArray() with
                | [| v0; v1; v2 |] ->
                    yield Triangle(v0, v1, v2)
                | _ -> failwith "Can not form a triangle."
        }
        
        let meshes =
            triangles
            |> Seq.cast
            |> Array.ofSeq
        
        BVH.create meshes tMin tMax
