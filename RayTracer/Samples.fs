module RayTracer.Samples

open RayTracer.Intersectable
open RayTracer.Material
open RayTracer.Texture

let randomScene () =
    let world = seq<IIntersectable> {
        let ground = Diffuse (SolidColor(Vec3d.make 0.5 0.5 0.5))
        yield Sphere(Vec3d.make 0.0 -1000.0 0.0, 1000.0, ground)
        
        let mat1 = Diffuse(SolidColor(Vec3d.make 0.4 0.2 0.1))
        yield Sphere(Vec3d.make -4.0 1.0 0.0, 1.0, mat1)
        let mat2 = Metal(SolidColor(Vec3d.make 0.7 0.6 0.5), 0.0)
        yield Sphere(Vec3d.make 4.0 1.0 0.0, 1.0, mat2)
    }
    
    let ratio = 3.0 / 2.0
    let width = 1200
    let height =
        double width / ratio
        |> int
    let spp = 50
    let lookFrom = Vec3d.make 13.0 2.0 3.0
    let lookAt = Vec3d.make 0.0 0.0 0.0
    let vup = Vec3d.make 0.0 1.0 0.0
    let distToFocus = 10.0
    let aperture = 0.0
    
    let cam = Camera.create lookFrom lookAt vup ratio (Angle.Degree 20.0) aperture distToFocus 0.0 0.0
    let background = Vec3d.make 0.7 0.8 1.0
    {
        Scene.camera = cam
        background = background
        spp = spp
        width = width; height = height
        objects = Array.ofSeq world
    }
    