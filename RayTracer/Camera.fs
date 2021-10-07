namespace RayTracer

open System

type Camera = {
    origin: Point3d
    lowerLeftCorner: Point3d
    horizontal: Vec3d
    vertical: Vec3d
    u: Vec3d; v: Vec3d; w: Vec3d
    lensRadius: double
    shutterOpen: double; shutterClose: double
} with
    static member create
            (lookFrom : Point3d) lookAt
            (vup : Vec3d) aspectRatio (fov : Angle) aperture focusDist
            shutterOpen shutterClose
        =
            
        let theta = fov.rad
        let h = Math.Tan (theta / 2.0)
        let viewportHeight = 2.0 * h
        let viewportWidth = aspectRatio * viewportHeight
        let w = (lookFrom - lookAt).normalized
        let u = vup.cross w
        let v = w.cross u
        let origin = lookFrom
        let horizontal = focusDist * viewportWidth * u
        let vertical = focusDist * viewportHeight * v;
        let lowerLeftCorner =
            origin - horizontal / 2.0 - vertical / 2.0 - focusDist * w;
        let lensRadius = aperture / 2.0
        
        {
            origin = origin
            lowerLeftCorner = lowerLeftCorner
            horizontal = horizontal
            vertical = vertical
            u = u; v = v; w = w
            lensRadius = lensRadius
            shutterOpen = shutterOpen
            shutterClose = shutterClose
        }
    
    member self.getRay (u : double) (v : double) =
        let radius = self.lensRadius * Vec3d.randomInUnitDisk()
        let offset = self.u * radius.x + self.v * radius.y
        
        {
            Ray.origin = self.origin + offset
            direction = self.lowerLeftCorner + u * self.horizontal + v * self.vertical - self.origin - offset
            time = RandomGenerator.randomRange self.shutterOpen self.shutterClose
        }
