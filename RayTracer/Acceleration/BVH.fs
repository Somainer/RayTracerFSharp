#nowarn "9"
namespace rec RayTracer.Acceleration.BVH

open System
open RayTracer
open RayTracer.Acceleration
open RayTracer.Intersectable

open System.Collections.Generic
open RayTracer.Material
open Microsoft.FSharp.NativeInterop

type TCandidates = HitRecord MutableArraySegment

type BVH = {
    root: BVHTree
    size: int
} with
    interface IIntersectable with
        member this.boundingBox _ _ = Some this.root.bounds
        member this.intersect(ray, tMin, tMax) =
            let candidatesPtr =
                this.size
                |> NativePtr.stackalloc<HitRecord>
                |> NativePtr.toVoidPtr
                
            let span = Span(candidatesPtr, this.size)
            let candidates = MutableArraySegment(span)
            
            BVHTree.getIntersectCandidates(this.root, &ray, tMin, tMax, &candidates)
            BVHTree.mergeIntersections(&ray, &candidates, tMin, tMax)

type BVHTree =
    | Leaf of bounds : AABB * object : IIntersectable
    | Branch of bounds : AABB * left : BVHTree * right : BVHTree

    member self.bounds : AABB =
        match self with
        | Leaf (bounds, _) -> bounds
        | Branch (bounds = bounds) -> bounds
    
module BVHTree =
    let rec getIntersectCandidates (tree, ray : Ray inref, tMin, tMax, candidates : TCandidates inref) =
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

    let mergeIntersections (ray : Ray inref, candidates : TCandidates inref, tMin, tMax) =
        let span = candidates.Span
        let mutable result = ValueNone
        for intersect in span do
            match intersect with
            | intersect when result.IsNone ->
                result <- ValueSome intersect
            | candidate ->
                let last = result.Value
                if intersect.t < last.t then
                    result <- ValueSome candidate
        
        result
        
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
        