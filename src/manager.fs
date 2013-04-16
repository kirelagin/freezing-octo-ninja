namespace Dalvik

module Manager =
    open System.Collections.Generic
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5
    open Shared

    [<JavaScript>]
    let mutable dexf : DexLoader.DexFile = As<DexLoader.DexFile> null //HACK!!!

    [<JavaScript>]
    let init (bytes : ArrayBuffer) =
        dexf <- DexLoader.DexFile.Read bytes

    [<JavaScript>]
    type VMObject = VMObj of Dex.Class * Dictionary<uint16, JsValue>

    [<JavaScript>]
    let heap : VMObject array = [| |]

    [<JavaScript>]
    let dereference ref =
        let inst = heap.[ref]
        if JavaScript.TypeOf inst = JavaScript.Undefined then failwith "Bad reference" else
        inst

    [<JavaScript>]
    let createInstance (cls : Dex.Class) =
        Array.push heap <| VMObj (cls, Dictionary ())
        heap.Length - 1

    [<JavaScript>]
    let classOfType (t : Dex.Type) =
        if JavaScript.TypeOf t.cls = JavaScript.Kind.Undefined then failwith "Type without associated class!" else
        t.cls

    [<JavaScript>]
    let resolveMethod (cls : Dex.Class) (name : string) (proto : Dex.Proto) =
        let c = ref cls
        let m = ref None
        while (!m).IsNone do
            m := Array.tryFind (fun (m : Dex.Method) -> m.name = name && m.proto = proto) ((!c).virtual_methods)
            if (!m).IsNone then
                c := match (!c).super with
                     | Some t -> classOfType t
                     | None -> failwith "Method not found" //TODO #10 throw IncompatibleClassChangeError
        (!m).Value

    [<JavaScript>]
    let processRequest (r : ResourceRequest) : ResourceReply =
        match r with
        | ResolveMethod (refr, name, proto) ->
            match dereference refr with
            | VMObj (cls, r) ->
                let resolved = resolveMethod cls name proto
                ProvideMethod resolved
        | CreateInstance dtype ->
            ProvideInstance << createInstance << classOfType <| dtype
