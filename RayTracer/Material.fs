namespace rec RayTracer.Material

open System.Runtime.CompilerServices
open RayTracer
open RayTracer.Texture

[<Struct>]
type HitRecord = {
    point: Point3d
    normal: Vec3d
    t: double
    u: double; v: double
    materialId: int
} with
    member self.material = ObjectPool.Find self.materialId :?> Material
module HitRecord =
    let inline withMaterial (material : Material) record =
        { record with
            materialId = material.InstanceId
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
                match hitRecord.normal + Vec3d.randomInUnitDisk().normalized with
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
