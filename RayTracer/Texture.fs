namespace RayTracer.Texture

open RayTracer

type ITexture =
    abstract member eval : u : double -> v : double -> p : Point3d -> Color3d 


type SolidColor (color) =
    interface ITexture with
        member _.eval _ _ _ = color
