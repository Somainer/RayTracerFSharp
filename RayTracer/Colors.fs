#nowarn "9"
namespace RayTracer.Colors

open System.Runtime.InteropServices
open RayTracer

[<Struct; StructLayout(LayoutKind.Explicit)>]
type Color = {
    [<FieldOffset(0); DefaultValue>] rgba: uint32
    [<FieldOffset(0)>] r: uint8
    [<FieldOffset(1)>] g: uint8
    [<FieldOffset(2)>] b: uint8
    [<FieldOffset(3)>] a: uint8
} with
    member inline this.rgb = this.rgba >>> (8 * sizeof<uint8>)
    member this.ToColor3d : Color3d =
        Vec3d.make
            ((double this.r) / 255.0)
            ((double this.g) / 255.0)
            ((double this.b) / 255.0)
