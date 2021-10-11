namespace rec RayTracer

open System
open System.Numerics
open System.Runtime.CompilerServices

[<Struct; NoComparison>]
type Vec3d = {
    [<CompiledName("X")>] x: double
    [<CompiledName("Y")>] y: double
    [<CompiledName("Z")>] z: double
}

module Vec3d =
    let inline make x y z = { x = x; y = y; z = z }
    
    let inline only x = make x x x
    let random () =
        make
            (RandomGenerator.nextDouble())
            (RandomGenerator.nextDouble())
            (RandomGenerator.nextDouble())
    
    let randomRange min max =
        make
            (RandomGenerator.randomRange min max)
            (RandomGenerator.randomRange min max)
            (RandomGenerator.randomRange min max)
            
    let randomInUnitDisk () =
        let p = make (RandomGenerator.randomRange -1.0 1.0) (RandomGenerator.randomRange -1.0 1.0) 0.0
        if p.normSquared >= 1.0 then p
        else randomInUnitDisk()
        
    let randomInUnitSphere () =
        let p = randomRange -1.0 1.0
        if p.normSquared >= 1.0 then randomInUnitSphere()
        else p

    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    let zero = only 0.0
    
    let elementwiseMax a b =
        make
            (Math.Max(a.x, b.x))
            (Math.Max(a.y, b.y))
            (Math.Max(a.z, b.z))
    
    let elementwiseMin a b =
        make
            (Math.Min(a.x, b.x))
            (Math.Min(a.y, b.y))
            (Math.Min(a.z, b.z))
        

type Vec3d with
    member this.Item
        with inline get index =
            match index with
            | 0 -> this.x
            | 1 -> this.y
            | 2 -> this.z
            | _ -> raise (IndexOutOfRangeException($"Of {index}"))
    
    static member inline (+) (a, b) = Vec3d.make (a.x + b.x) (a.y + b.y) (a.z + b.z)
    static member inline (-) (a, b) = Vec3d.make (a.x - b.x) (a.y - b.y) (a.z - b.z)
    static member inline (*) (a, b) = Vec3d.make (a.x * b) (a.y * b) (a.z * b)
    static member inline (*) (a, b) = Vec3d.make (b.x * a) (b.y * a) (b.z * a)
    static member inline (*) (a, b) = Vec3d.make (a.x * b.x) (a.y * b.y) (a.z * b.z)
    static member inline (/) (a, b) = Vec3d.make (a.x / b) (a.y / b) (a.z / b)
    static member inline (~-) self = Vec3d.make -self.x -self.y -self.z
    
    [<CompiledName("Dot")>]
    member x.dot y = x.x * y.x + x.y * y.y + x.z * y.z
    [<CompiledName("NormSquared")>]
    member self.normSquared = self.x * self.x + self.y * self.y + self.z * self.z
    [<CompiledName("Norm")>]
    member self.norm = Math.Sqrt self.normSquared
    [<CompiledName("Normalized")>]
    member self.normalized =
        self * (1.0 / self.norm)
    
    [<CompiledName("Cross")>]
    member self.cross rhs =
        Vec3d.make
            (self.y * rhs.z - self.z * rhs.y)
            (self.z * rhs.x - self.x * rhs.z)
            (self.x * rhs.y - self.y * rhs.x)
    
    static member op_Implicit self = Vector3(float32 self.x, float32 self.y, float32 self.z)
    static member op_Implicit (vec3 : Vector3) = Vec3d.make (float vec3.X) (float vec3.Y) (float vec3.Z) 
    
    member self.nearZero =
        Util.nearZero self.x
        && Util.nearZero self.y
        && Util.nearZero self.z
        
    member self.reflect n =
        self - 2.0 * self.dot(n) * n
        
    member self.refract (n : Vec3d) (ratio : float) =
        let cosTheta =
            (-self).dot(n)
            |> min 1.0
        let rOutPerpendicular = ratio * (self + cosTheta * n)
        let rOutParallel =
            1.0 - rOutPerpendicular.normSquared
            |> abs
            |> sqrt
            |> neg
            |> (*) n
            
        rOutPerpendicular + rOutParallel


type Point3d = Vec3d
type Color3d = Vec3d
