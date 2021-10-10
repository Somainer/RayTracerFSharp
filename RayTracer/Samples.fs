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
        let mat3 = Dielectric(1.5)
        yield Sphere(Vec3d.make 0.0 1.0 0.0, 1.0, mat3)
        
        for a in -11..11 do
            for b in -11..11 do
                let chooseMaterial = RandomGenerator.nextDouble()
                let center = Vec3d.make (double a + 0.9) 0.2 (double b + 0.9 * RandomGenerator.nextDouble())
                if (center - (Vec3d.make 4.0 0.2 0.0)).norm > 0.9 then
                    let material : Material =
                        if chooseMaterial < 0.8 then
                            let albedo = Vec3d.random() * Vec3d.random()
                            upcast Diffuse(SolidColor(albedo))
                        else if chooseMaterial < 0.95 then
                            let albedo = Vec3d.randomRange 0.5 1.0
                            let fuzz = RandomGenerator.randomRange 0.0 0.5
                            upcast Metal(SolidColor(albedo), fuzz)
                        else
                            upcast Dielectric(1.0 + (RandomGenerator.randomRange 0.2 0.9))
                    
                    yield Sphere(center, 0.2, material)

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
    