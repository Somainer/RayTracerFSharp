namespace rec RayTracer.Acceleration.BVH

open System
open RayTracer
open RayTracer.Acceleration
open RayTracer.Intersectable

open System.Collections.Generic
open RayTracer.Material
open Microsoft.FSharp.NativeInterop

type TCandidates = IntersectionAccumulator

type BVH = {
    root: BVHTree
    size: int
} with
    interface IIntersectable with
        member this.boundingBox _ _ = Some this.root.bounds
        member this.intersect(ray, tMin, tMax) =
            let mutable candidates = TCandidates()
            
            BVHTree.getIntersectCandidates(this.root, &ray, tMin, tMax, &candidates)
            candidates.Result

type BVHTree =
    | Leaf of bounds : AABB * object : IIntersectable
    | Branch of bounds : AABB * left : BVHTree * right : BVHTree

    member self.bounds : AABB =
        match self with
        | Leaf (bounds, _) -> bounds
        | Branch (bounds = bounds) -> bounds
    
module BVHTree =
    let rec getIntersectCandidates (tree, ray : Ray inref, tMin, tMax, candidates : TCandidates byref) =
        match tree with
        | Leaf (bounds, object) -> 
            if bounds.isIntersect(&ray, tMin, tMax) then
                let intersect = object.intersect(&ray, tMin, tMax)
                if intersect.IsSome then
                    candidates.Add (intersect.Value)
        | Branch (bounds, left, right) ->
            if bounds.isIntersect(&ray, tMin, tMax) then
                getIntersectCandidates(left, &ray, tMin, tMax, &candidates)
                getIntersectCandidates(right, &ray, tMin, tMax, &candidates)
        
module BVH =
    let private getLeaves (list : IIntersectable[]) tMin tMax =
        let axis = RandomGenerator.randomInt 0 2
        list
        |> Array.map (fun o -> Leaf ((o.boundingBox tMin tMax).Value, o))
        |> Array.sortWith (fun a b ->
            let boxA = a.bounds
            let boxB = b.bounds
            
            boxA.Minimum.[axis].CompareTo boxB.Minimum.[axis]
        )
        
    let rec private recursiveBuild (list : BVHTree Span) =
        let len = list.Length
        if len = 1 then list.[0]
        else
            let mid = len / 2
            let left = recursiveBuild (list.Slice(0, mid))
            let right = recursiveBuild (list.Slice(mid))
            let bounds = left.bounds.surroundWith right.bounds
            Branch(bounds, left, right)
            
    let create list tMin tMax =
        let leaves = getLeaves list tMin tMax
        let root = recursiveBuild (leaves.AsSpan())
        
        {
            BVH.root = root
            size = leaves.Length
        }
        