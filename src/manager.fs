namespace Dalvik

module Manager =
    open System.Collections.Generic
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5
    open Shared

    [<JavaScript>]
    let library : Dictionary<Dex.Type, Dex.Class> = Dictionary ()

    [<JavaScript>]
    let registerClass (cls : Dex.Class) =
        library.Add(cls.dclass, cls)

    [<JavaScript>]
    let loadDex (bytes : ArrayBuffer) =
        DexLoader.DexFile.Read bytes registerClass

    [<JavaScript>]
    let init = loadDex

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
        match Dictionary.tryGet library t with
        | Some c -> c
        | None -> failwith "Type without associated class!"

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
        | RequestClass (dtype) ->
            match Dictionary.tryGet library dtype with
            | Some c -> ProvideClass c
            | None -> failwith <| "Unknown class: " + dtype.descriptor
        | ResolveMethod (refr, name, proto) ->
            match dereference refr with
            | VMObj (cls, r) ->
                let resolved = resolveMethod cls name proto
                ProvideMethod resolved
        | CreateInstance dtype ->
            ProvideInstance << createInstance << classOfType <| dtype
