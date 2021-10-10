namespace RayTracer

open System
open System.Diagnostics
open System.Threading.Tasks
open System.Linq
open RayTracer.Acceleration.BVH
open RayTracer.Colors
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
//    member self.bvh = IntersectableList self.objects
    
    member self.renderSequential () =
        let watch = Stopwatch.StartNew()
        let bvh = self.bvh
        printfn $"BVH built in {watch.ElapsedMilliseconds}ms"
        watch.Restart()
        let image = seq {
            for j in 0..self.height do
                printfn $"%.3f{double (j + 1) / double self.height}%% Completed.\r"
                for i in 0..self.width do
                    yield self.sampledRenderSingle bvh i j
        }
        let buf =
            image
            |> Array.ofSeq
        printfn $"Rendering completed in {watch.Elapsed} ms"
        
        {
          RenderResult.height = self.height
          width = self.width
          spp = self.spp
          pixels = buf
        }
    
    member self.render () =
        let watch = Stopwatch.StartNew()
        let image = Array2D.create self.height self.width Vec3d.zero
        let bvh = self.bvh
        // printfn and string is not aot-friendly, using string.format instead.
        Console.WriteLine ("BVH built in {0} ms", watch.ElapsedMilliseconds.ToString())
        let mutable finishCount = 0
        watch.Restart()
        let result =
            Parallel.For(0, self.height,
                Action<_> (fun j ->
                    Parallel.For(0, self.width,
                        Action<_> (fun i ->
                            Array2D.set image j i (self.sampledRenderSingle bvh i j))
                    ) |> ignore
                    let completed = Threading.Interlocked.Increment(&finishCount)
                    Console.Write("{0:P} Completed\r", double completed / double self.height)))
        ignore result
        Console.WriteLine("Rendering completed in {0}", watch.Elapsed.ToString())
        let buf =
            image.Cast()
            |> Array.ofSeq
        
        {
          RenderResult.height = self.height
          width = self.width
          spp = self.spp
          pixels = buf
        }
        
    member self.RenderWithCallback (callback : Action<int, int, Color>) (doneCallback : Color3d[] Action) =
        let bvh = self.bvh
        
        let image = Array2D.create self.height self.width Vec3d.zero
        let watch = Stopwatch.StartNew()
        Parallel.For(0, self.height,
            Action<_> (fun j ->
                Parallel.For(0, self.width,
                    Action<_> (fun i ->
                        let pixel =
                            self.sampledRenderSingle bvh i j
                        let color = Color.correctedColor pixel
                        Array2D.set image j i pixel
                        callback.Invoke(j, i, color)
                )) |> ignore)) |> ignore

        doneCallback.Invoke(image.Cast() |> Array.ofSeq)
        printfn $"Render completed in {watch.Elapsed}"

    member self.EvolvingRendering (callback : Action<int, int, Color>) (doneCallback : Action<int, Color3d[,], Action>) =
        let bvh = self.bvh
        let image  = Array2D.create self.height self.width Vec3d.zero
        
        let watch = Stopwatch.StartNew()
        let mutable spp = 0
        let mutable shouldRun = true
        
        let stop = Action(fun () -> shouldRun <- false)
        
        while shouldRun do
            spp <- spp + 1
            Parallel.For(0, self.height,
                Action<_> (fun j ->
                    Parallel.For(0, self.width,
                        Action<_> (fun i ->
                            let pixel =
                                self.renderSingle bvh i j
                            let lastPixel = Array2D.get image j i
                            let final = (pixel + lastPixel * (float (spp - 1))) / float spp
                            Array2D.set image j i final
                            let color = Color.correctedColor final
                            callback.Invoke(j, i, color)
                    )) |> ignore)) |> ignore
            doneCallback.Invoke(spp, image, stop)
            printf $"Spp: {spp} Elapsed: {watch.Elapsed}\r"
