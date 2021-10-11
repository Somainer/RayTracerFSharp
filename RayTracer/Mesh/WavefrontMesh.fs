namespace rec RayTracer.Mesh

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open RayTracer

type WavefrontMesh = {
    vertices: Vec3d IImmutableList
    normals : Vec3d IImmutableList
    uvw : Vec3d IImmutableList
    
    faceVerticesIndices: int IImmutableList IImmutableList
    faceUVWIndices: int IImmutableList IImmutableList
    faceNormalsIndices: int IImmutableList IImmutableList
} with
    member self.face with get index =
        self.faceVerticesIndices.[index]
        |> Seq.map (fun i -> self.vertices.[i])
        |> ImmutableList.CreateRange
    
    member self.faces =
        self.faceVerticesIndices
        |> Seq.map (
            Seq.map (fun i -> self.vertices.[i]) >> ImmutableList.CreateRange)
        
    static member LoadFromObj (reader : StreamReader) =
        let verticesIndexes : _ IImmutableList List = List()
        let uvwIndexes : _ IImmutableList List = List()
        let normalsIndexes : _ IImmutableList List = List()
        let vertices = List()
        let normals = List()
        let uvw = List()
        
        let inline read (reader : StreamReader) =
            match reader.ReadLine() with
            | null -> None
            | line -> Some(line, reader)
            
        for line in Seq.unfold read reader do
            match WavefrontMesh.parseLine line with
            | Face (vertIndex, uvwIndex, normalIndex) ->
                verticesIndexes.Add(vertIndex.ToImmutableList())
                uvwIndexes.Add(uvwIndex.ToImmutableList())
                normalsIndexes.Add(normalIndex.ToImmutableList())
            | Vertex vert -> vertices.Add vert
            | Normal n -> normals.Add n
            | UVW u -> uvw.Add u
            | Ignore -> ()
        
        {
            vertices = vertices.ToImmutableList()
            normals = normals.ToImmutableList()
            uvw = uvw.ToImmutableList()
            
            faceVerticesIndices = verticesIndexes.ToImmutableList()
            faceNormalsIndices = normalsIndexes.ToImmutableList()
            faceUVWIndices = uvwIndexes.ToImmutableList()
        }
        

type WavefrontParseResult =
    | Face of vertIndex : int List * uvwIndex : int List * normalIndex : int List
    | Vertex of Vec3d
    | Normal of Vec3d
    | UVW of Vec3d
    | Ignore
    
module WavefrontMesh =
    let internal parseLine line =
        if String.IsNullOrWhiteSpace line then Ignore
        else
            let tokens = line.Split(' ').AsSpan()
            let payload = tokens.Slice 1
            let inline parseIndex s = (parse s) - 1
            match tokens.[0] with
            | "f" ->
                let vertIndex = List()
                let uvwIndex = List()
                let normalIndex = List()
                let faceInfo = tokens.Slice(1)
                for info in faceInfo do
                    match info.Split('/') with
                    | [| v |] -> vertIndex.Add(parseIndex v)
                    | [| v; u |] ->
                        vertIndex.Add (parseIndex v)
                        uvwIndex.Add (parseIndex u)
                    | [| v; u; n |] ->
                        vertIndex.Add (parseIndex v)
                        uvwIndex.Add (parseIndex u)
                        normalIndex.Add (parseIndex n)
                    | _ -> ()
                Face(vertIndex, uvwIndex, normalIndex)
            
            | "v" -> parseVec3d(&payload) |> Vertex
            | "vn" -> parseVec3d(&payload) |> Normal
            | "vt" ->
                let xs = payload.[0]
                let ys = payload.[1]
                Vec3d.make (parse xs) (parse ys) 0.0
                |> UVW
            | _ -> Ignore
            
    
    let private parseVec3d (span : string Span inref) =
        let xs = span.[0]
        let ys = span.[1]
        let zs = span.[2]
        
        Vec3d.make (parse xs) (parse ys) (parse zs)
