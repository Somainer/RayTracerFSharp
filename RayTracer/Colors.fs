#nowarn "9"
namespace RayTracer.Colors

open System.Runtime.InteropServices
open RayTracer

[<Struct; StructLayout(LayoutKind.Explicit)>]
type Color = {
    [<FieldOffset(0)>] rgba: int32
    [<FieldOffset(0)>] r: int8
    [<FieldOffset(1)>] g: int8
    [<FieldOffset(2)>] b: int8
    [<FieldOffset(3)>] a: int8
} with
    member inline this.rgb = this.rgba >>> (8 * sizeof<int8>)
    member this.ToColor3d : Color3d =
        Vec3d.make
            ((double this.r) / 255.0)
            ((double this.g) / 255.0)
            ((double this.b) / 255.0)
