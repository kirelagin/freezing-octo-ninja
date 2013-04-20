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
    type VMObject = VMObj of Dex.Class * Dictionary<Dex.Field, RegValue>

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
    let initialisedClasses : Dictionary<Dex.Type, Dictionary<Dex.Field, RegValue>> = Dictionary ()

    [<JavaScript>]
    let ensureClassInitialised (dtype : Dex.Type) (cont : Dictionary<Dex.Field, RegValue> -> unit) =
        match Dictionary.tryGet initialisedClasses dtype with
        | Some d -> cont d
        | None ->
            let d = Dictionary ()
            initialisedClasses.Add(dtype, d)
            let cls = classOfType dtype
            let clinit = Dex.Method (dtype, Proto ("()V", Type "V", [| |]), "<clinit")
            match Runtime.getMethodImpl cls true clinit with
            | None -> cont <| d
            | Some impl -> (new ThreadWorker.Thread ()).ExecuteMethod (dtype, impl) [| |] (fun () -> cont d) 

    [<JavaScript>]
    let rec resolveMethod (cls : Dex.Class) (meth : Dex.Method)  =
        let (Class (dtype, _, super, _, impl)) = cls
        match impl with
        | None -> failwith "Class without class_data"
        | Some (ClassImpl (_, _, _, virt)) ->
            let m = Dumbdict.tryGetWith (fun (Method (_, p1, n1)) (Method (_, p2, n2)) -> n1 = n2 && p1 = p2) virt meth
            match m with
            | Some (_, Some method_impl) -> (dtype, method_impl)
            | Some (_, None) -> failwith "Method not implemented" //TODO #10 throw IncompatibleClassChangeError
            | None ->
                match super with
                | None -> failwith "Method not found" //TODO #10 throw IncompatibleClassChangeError
                | Some t -> resolveMethod (classOfType t) meth

    [<JavaScript>]
    let processRequest (r : ResourceRequest, cont : ResourceReply -> unit) =
        match r with
        | RequestClass (dtype) ->
            match Dictionary.tryGet library dtype with
            | Some c ->
                ensureClassInitialised dtype (fun _ -> cont <| ProvideClass c)
            | None -> let (Type descr) = dtype in failwith <| "Unknown class: " + descr
        | ResolveMethod (refr, meth) ->
            match dereference refr with
            | VMObj (cls, r) ->
                let resolved = resolveMethod cls meth
                cont <| ProvideMethod resolved
        | CreateInstance dtype ->
            cont << ProvideInstance << createInstance << classOfType <| dtype
        | GetInstanceField (refr, f) ->
            let (VMObj (_, d)) = heap.[refr]
            cont << ProvideValue <| d.[f]
        | PutInstanceField (refr, f, v) ->
            let (VMObj (_, d)) = heap.[refr]
            d.Add (f, v)
            cont RequestProcessed
        | GetStaticField f ->
            let (Field (dtype, _, _)) = f
            ensureClassInitialised dtype (fun d ->
                                            cont << ProvideValue <| d.[f])
        | PutStaticField (f, v) ->
            let (Field (dtype, _, _)) = f
            ensureClassInitialised dtype (fun d ->
                                            d.Add (f, v)
                                            cont RequestProcessed)
