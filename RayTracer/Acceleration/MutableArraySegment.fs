namespace RayTracer.Acceleration

open System
open System.Runtime.CompilerServices

[<Struct; IsByRefLike>]
type MutableArraySegment<'a> =
    struct
        val data : 'a Span
        val mutable current : int
        new (data) = { data = data; current = 0; }
        
        member self.Length = self.current
        member self.Add element =
            self.data.[self.current] <- element
            self.current <- self.current + 1
        
        member self.Span = self.data.Slice(0, self.Length)
    end

