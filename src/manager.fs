namespace Dalvik

module Manager =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5
    open Shared

    [<JavaScript>]
    let mutable dexf : Dex.DexFile = As<Dex.DexFile> null //HACK!!!

    [<JavaScript>]
    let init (bytes : ArrayBuffer) =
        dexf <- Dex.DexFile.Read bytes

    [<JavaScript>]
    type VMObject = VMObj of Dex.Class * ObjectMap.ObjectMap<uint16, JsValue>

    [<JavaScript>]
    let heap : VMObject array = [| |]

    [<JavaScript>]
    let dereference ref =
        let inst = heap.[ref]
        if JavaScript.TypeOf inst = JavaScript.Undefined then failwith "Bad reference" else
        inst

    [<JavaScript>]
    let createInstance (cls : Dex.Class) =
        Array.push heap <| VMObj (cls, ObjectMap.empty)
        heap.Length - 1

    [<JavaScript>]
    let classOfType (t : Dex.Type) =
        match t.Cls with
        | Some c -> c
        | None -> failwith "Type without associated class!"

    [<JavaScript>]
    let resolveMethod (cls : Dex.Class) (meth : Dex.Method) =
        let c = ref cls
        let m = ref None
        while (!m).IsNone do
            m := Array.tryFind (fun (m : Dex.Method) -> meth.Name = m.Name && meth.Proto = m.Proto) ((!c).VirtualMethods)
            if (!m).IsNone then
                c := match (!c).Super with
                     | Some t -> classOfType t
                     | None -> failwith "Method not found" //TODO #10 throw IncompatibleClassChangeError
        (!m).Value

    [<JavaScript>]
    let processRequest (r : ResourceRequest) : ResourceReply =
        match r with
        | RequestMethod midx ->
            ProvideMethod dexf.Methods.[int midx]
        | ResolveMethod (refr, midx) ->
            match dereference refr with
            | VMObj (cls, r) ->
                let meth = dexf.Methods.[int midx]
                let resolved = resolveMethod cls meth
                ProvideMethod resolved
        | CreateInstance tidx ->
            ProvideInstance << createInstance << classOfType <| dexf.Types.[int tidx]
