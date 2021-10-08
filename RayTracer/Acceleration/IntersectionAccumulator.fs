namespace RayTracer.Acceleration

open System.Runtime.CompilerServices
open RayTracer.Material

[<IsByRefLike>]
type IntersectionAccumulator =
    struct
        val mutable private candidate : HitRecord voption
        member self.Add c =
            match self.candidate with
            | ValueNone -> self.candidate <- ValueSome c
            | ValueSome prev when prev.t > c.t ->
                self.candidate <- ValueSome c
            | _ -> ()
            
        member self.Result = self.candidate
    end
