module RayTracer.Program


[<EntryPoint>]
let main args =
    let scene = Samples.randomScene()
//    let result = scene.renderSequential()
    let result = scene.render()
    let path = if args.Length = 0 then "scene.ppm" else args.[0]
    result.WriteToPPMFile path
    0
