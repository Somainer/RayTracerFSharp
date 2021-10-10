namespace rec RayTracer.Material

open System
open System.Runtime.CompilerServices
open RayTracer
open RayTracer.Texture

[<Struct>]
type HitRecord = {
    point: Point3d
    normal: Vec3d
    t: double
    u: double; v: double
    material: Material
    frontFace: bool
}
module HitRecord =
    let inline withMaterial (material : Material) record =
        { record with
            material = material
        }
        
    let inline newWithFaceNormal t point u v outwardNormal material (ray : _ inref) =
        let frontFace = ray.direction.dot outwardNormal < 0.0
        let normal = if frontFace then outwardNormal else -outwardNormal
        {
            point = point
            normal = normal
            t = t
            u = u; v = v
            material = material
            frontFace = frontFace
        }

[<AbstractClass>]
type Material () =
    inherit Object ()
    abstract member scatter : rayIn : Ray inref * hitRecord : HitRecord inref -> ValueOption<struct (Color3d * Ray)>
    abstract member emitted : u : double -> v : double -> p : Point3d -> Color3d

type Diffuse<'t when 't :> ITexture> (albedo : 't) =
    inherit Material () with
        override this.scatter(rayIn, hitRecord) =
            let scatterDirection =
                match hitRecord.normal + Vec3d.randomInUnitSphere().normalized with
                | m when m.nearZero -> hitRecord.normal
                | m -> m
                
            let color = albedo.eval hitRecord.u hitRecord.v hitRecord.point
            let ray = {
                rayIn with
                    origin = hitRecord.point
                    direction = scatterDirection
            }
            
            ValueSome ((color, ray))
        override this.emitted _ _ _ = Vec3d.zero
        

type Metal<'t when 't :> ITexture> (albedo : 't, fuzz : double) =
    inherit Material () with
        override this.scatter(rayIn, hitRecord) =
            let reflected = rayIn.direction.normalized.reflect(hitRecord.normal)
            let fuzzed = reflected + fuzz * Vec3d.randomInUnitSphere()
            
            if fuzzed.dot hitRecord.normal > 0.0 then
                let color = albedo.eval hitRecord.u hitRecord.v hitRecord.point
                let ray = {
                    rayIn with
                        origin = hitRecord.point
                        direction = fuzzed
                }
                ValueSome ((color, ray))
            else ValueNone
        override this.emitted _ _ _ = Vec3d.zero

module Material =
    let Dummy = { 
        new Material () with
            member _.scatter(_, _) = ValueNone
            member _.emitted _ _ _ = Vec3d.zero
    }

type Dielectric (indexRefraction) =
    inherit Material ()
    static member reflectance cosine refIdx =
        let r = (1.0 - refIdx) / (1.0 + refIdx)
        let r0 = r * r
        
        r0 + (1.0 - r0) * Math.Pow(1.0 - cosine, 5.0)

    override this.emitted _ _ _ = Vec3d.zero
    override self.scatter(rayIn, hitRecord) =
        let refractionRatio =
            if hitRecord.frontFace then 1.0 / indexRefraction
            else indexRefraction
        let unitRedirection = rayIn.direction.normalized
        let cosTheta =
            ((-unitRedirection).dot(hitRecord.normal))
            |> min 1.0
        let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
        
        let fullReflect = refractionRatio * sinTheta > 1.0
        let direction =
            if fullReflect || Dielectric.reflectance cosTheta refractionRatio > RandomGenerator.nextDouble() then
                unitRedirection.reflect hitRecord.normal
            else unitRedirection.refract hitRecord.normal refractionRatio
        
        let ray = {
            rayIn with
                origin = hitRecord.point
                direction = direction
        }
        ValueSome (Vec3d.only 1.0, ray)
