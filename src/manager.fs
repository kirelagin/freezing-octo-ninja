namespace Dalvik

module Manager =
    open System.Collections.Generic
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5
    open Dex
    open Shared

    [<JavaScript>]
    let library : Dictionary<Dex.Type, Dex.Class> = Dictionary ()

    [<JavaScript>]
    let loadedFiles : DexLoader.DexFile array = [| |]

    [<JavaScript>]
    let registerClass (cls : Dex.Class) =
        let (Class (dtype, _, _, _, _)) = cls
        library.Add(dtype, cls)

    [<JavaScript>]
    let loadDex (bytes : ArrayBuffer) =
        Array.push loadedFiles <| DexLoader.DexFile.Read bytes registerClass

    [<JavaScript>]
    let init = loadDex

    [<JavaScript>]
    type VMObject = VMObj of Dex.Class * Dictionary<uint16, RegValue>

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
    let rec resolveMethod (cls : Dex.Class) (meth : Dex.Method) =
        let (Class (dtype, _, super, _, impl)) = cls
        match impl with
        | None -> failwith "Class without class_data"
        | Some (ClassImpl (_, _, direct, virt)) ->
            let m = Dumbdict.tryGet virt meth
            match m with
            | Some (_, Some method_impl) -> (dtype, method_impl)
            | Some (_, None) -> failwith "Method not implemented" //TODO #10 throw IncompatibleClassChangeError
            | None ->
                match super with
                | None -> failwith "Method not found" //TODO #10 throw IncompatibleClassChangeError
                | Some t -> resolveMethod (classOfType t) meth

    [<JavaScript>]
    let processRequest (r : ResourceRequest) : ResourceReply =
        match r with
        | RequestClass (dtype) ->
            match Dictionary.tryGet library dtype with
            | Some c -> ProvideClass c
            | None -> let (Type descr) = dtype in failwith <| "Unknown class: " + descr
        | ResolveMethod (refr, meth) ->
            match dereference refr with
            | VMObj (cls, r) ->
                let resolved = resolveMethod cls meth
                ProvideMethod resolved
        | CreateInstance dtype ->
            ProvideInstance << createInstance << classOfType <| dtype
