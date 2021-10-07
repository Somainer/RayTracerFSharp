namespace rec RayTracer

open System.Collections.Generic

type Object () as self =   
    let m_InstanceId : int = ObjectPool.Register self
    member self.InstanceId = m_InstanceId
    
module ObjectPool =
    let private objects : Object List = List ()
    let Register obj =
        let id = objects.Count
        objects.Add obj
        id
    
    let Find id = objects.[id]
