#nowarn "9"
namespace RayTracer

open System
open System.IO
open System.Runtime.CompilerServices
open System.Text
open RayTracer.Colors

[<Struct; IsByRefLike>]
type RenderResult = {
    height: int
    width: int
    pixels: Color3d []
    spp: int
} with
    member self.WriteToPPMFile file =
        use stream = File.OpenWrite file
        use writer = new BinaryWriter(stream)
        
        let buf =
            self.pixels
            |> Array.map Color.correctedColor
        
        writer.Write (Encoding.ASCII.GetBytes (String.Format("P6\n{0} {1}\n255\n", self.width, self.height)))
        for pixel in buf do
            writer.Write pixel.r
            writer.Write pixel.g
            writer.Write pixel.b
