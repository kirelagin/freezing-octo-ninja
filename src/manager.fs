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
        library.[dtype] <- cls

    [<JavaScript>]
    let loadDex (bytes : ArrayBuffer) =
        Array.push loadedFiles <| DexLoader.DexFile.Read bytes registerClass

    [<JavaScript>]
    let init = loadDex

    [<JavaScript>]
    type VMObject = | VMInstance of Dex.Class * Dictionary<Dex.Field, RegValue>
                    | VMArray of Dex.JavaType * RegValue array

    [<JavaScript>]
    let heap : VMObject array = [| |]

    [<JavaScript>]
    let dereference ref =
        let inst = heap.[ref]
        if JavaScript.TypeOf inst = JavaScript.Undefined then failwith "Bad reference" else
        inst

    [<JavaScript>]
    let createInstance (cls : Dex.Class) =
        Array.push heap <| VMInstance (cls, Dictionary ())
        heap.Length - 1

    [<JavaScript>]
    let createArray (dtype : Dex.Type, size : int) =
        match Runtime.javatypeOfType dtype with
        | JavaReferenceType (ArrayType t) -> Array.push heap <| VMArray (t, Array.create size JavaScript.Undefined)
        | _ -> failwith "Bad array type"
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
            let clinit = Dex.Method (dtype, Proto ("V", Type "V", [| |]), "<clinit>")
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
            | VMInstance (cls, r) ->
                let resolved = resolveMethod cls meth
                cont <| ProvideMethod resolved
            | _ -> failwith "Instance expected"
        | CreateInstance dtype ->
            cont << ProvideInstance << createInstance << classOfType <| dtype
        | CreateArray (size, dtype) ->
            cont << ProvideInstance <| createArray (dtype, size)
        | FillArray (refr, vals) ->
            match heap.[refr] with
            | VMArray (t, a) ->
                let n = Array.append vals (Array.sub a vals.Length (a.Length - vals.Length))
                heap.[refr] <- VMArray (t, n)
                cont RequestProcessed
            | _ -> failwith "Array expected"
        | GetArrayItem (refr, i) ->
            match heap.[refr] with
            | VMArray (_, a) -> cont << ProvideValue <| a.[i]
            | _ -> failwith "Array expected"
        | PutArrayItem (refr, i, v) ->
            match heap.[refr] with
            | VMArray (t, a) ->
                a.[i] <- v
                cont RequestProcessed
            | _ -> failwith "Array expected"
        | GetInstanceField (refr, f) ->
            match heap.[refr] with
            | VMInstance (_, d) -> cont << ProvideValue <| d.[f]
            | _ -> failwith "Instance expected"
        | PutInstanceField (refr, f, v) ->
            match heap.[refr] with
            | VMInstance (_, d) -> d.[f] <- v
                                   cont RequestProcessed
            | _ -> failwith "Instance expected"
        | GetStaticField f ->
            let (Field (dtype, _, _)) = f
            ensureClassInitialised dtype (fun d ->
                                            cont << ProvideValue <| d.[f])
        | PutStaticField (f, v) ->
            let (Field (dtype, _, _)) = f
            ensureClassInitialised dtype (fun d ->
                                            d.[f] <- v
                                            cont RequestProcessed)
