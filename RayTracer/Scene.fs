namespace RayTracer

open System
open System.Threading.Tasks
open RayTracer.Acceleration.BVH
open RayTracer.Intersectable

type Scene = {
    height: int
    width: int
    objects: IIntersectable[]
    camera: Camera
    spp: int
    background: Color3d
} with
    member self.renderSingle bvh i j =
        let u = (double i + RandomGenerator.nextDouble()) / double (self.width - 1)
        let v = 1.0 - (double j + RandomGenerator.nextDouble()) / double (self.height - 1)
        let ray = self.camera.getRay u v
        Render.traceRay &ray bvh self.background 50
        
    member inline self.sampledRenderSingle bvh i j =
        let mutable color = Vec3d.zero
        for _ in 0..self.spp do
            color <- color + self.renderSingle bvh i j
        color / double self.spp
        
    member self.bvh = BVH.create self.objects self.camera.shutterOpen self.camera.shutterClose
    
    member self.render () =
        let image = Array2D.create self.width self.height Vec3d.zero
        let bvh = self.bvh
        let result =
            Parallel.For(0, self.height,
                Action<_> (fun j ->
                    Parallel.For(0, self.width,
                        Action<_> (fun i ->
                            Array2D.set image i j (self.sampledRenderSingle bvh i j))                    
                    ) |> ignore))
        ignore result
