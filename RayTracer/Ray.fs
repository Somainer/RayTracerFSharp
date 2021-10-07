namespace RayTracer

[<Struct>]
type Ray = {
    origin: Point3d
    direction: Vec3d
    time: double
} with
    member inline self.at time = self.origin + self.direction * time
