#nowarn "9"
namespace RayTracer.Colors

open RayTracer

[<Struct>]
type Color = {
    r: uint8
    g: uint8
    b: uint8
    a: uint8
} with
    member inline this.rgba = (uint this.r <<< 24) ||| (uint this.g <<< 16) ||| (uint this.b <<< 8) ||| uint this.a
    member inline this.rgb = this.rgba >>> (8 * sizeof<uint8>)
    member this.ToColor3d : Color3d =
        Vec3d.make
            ((double this.r) / 255.0)
            ((double this.g) / 255.0)
            ((double this.b) / 255.0)

module Color =
    let inline private normalizePixel c =
        Util.clamp c 0.0 0.999
        |> (*) 256.0
        |> uint8
        
    let correctedColor color =
        let inline normalize c =
            Util.gammaCorrection c 1.0
            |> normalizePixel
        
        let r = normalize color.x
        let g = normalize color.y
        let b = normalize color.z
        
        {
            r = r; g = g; b = b; a = 255uy
        }
