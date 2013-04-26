namespace Dalvik

module Manager =
    open System.Collections.Generic
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5
    open Dex
    open Shared

    type LoadedClass = | JustLoaded of Dex.Class
                       | Initialised of Dex.Class * Dictionary<Dex.Field, RegValue>

    [<JavaScript>]
    let library : Dictionary<Dex.Type, LoadedClass> = Dictionary ()

    [<JavaScript>]
    let loadedFiles : DexLoader.DexFile array = [| |]

    [<JavaScript>]
    let registerClass (cls : Dex.Class) =
        let (Class (dtype, _, _, _, _)) = cls
        library.[dtype] <- JustLoaded cls

    [<JavaScript>]
    let loadDex (bytes : ArrayBuffer) =
        Array.push loadedFiles <| DexLoader.DexFile.Read bytes registerClass

    [<JavaScript>]
    let init = loadDex


    [<JavaScript>]
    let ensureClassInitialised (t : Dex.Type) (cont : Dex.Class * Dictionary<Dex.Field, RegValue> -> unit) =
        match Dictionary.tryGet library t with
        | Some (Initialised (c, d)) -> cont (c, d)
        | Some (JustLoaded c) ->
            let d = Dictionary ()
            library.[t] <- Initialised (c, d)
            match c with
            | Class (_, _, _, _, Some (ClassImpl (fs, _, _, _))) ->
                Array.iter (fun f -> d.[f] <- Runtime.defaultValue f) <| Dumbdict.keys fs
                let clinit = Dex.Method (t, Proto ("V", Type "V", [| |]), "<clinit>")
                match Runtime.getMethodImpl c true clinit with
                | None -> cont <| (c, d)
                | Some impl -> (new ThreadWorker.Thread ()).ExecuteMethod (t, impl) [| |] (fun () -> cont (c, d)) 
            | Class (_, _, _, _, None) -> cont (c, d)
        | None -> failwith "Type without associated class!"

    [<JavaScript>]
    let classOfType (t : Dex.Type) (cont : Dex.Class -> unit) =
        ensureClassInitialised t <| fun (c, _) -> cont c

    [<JavaScript>]
    let classStaticData (t : Dex.Type) (cont : Dictionary<Dex.Field, RegValue> -> unit) =
        ensureClassInitialised t <| fun (_, d) -> cont d


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
    let createInstance (t : Dex.Type) (cont : dref -> unit) =
        classOfType t <| fun c ->
            Array.push heap <| VMInstance (c, Dictionary ())
            cont <| heap.Length - 1

    [<JavaScript>]
    let createArray (dtype : Dex.Type, size : int) =
        match Runtime.javatypeOfType dtype with
        | JavaReferenceType (ArrayType t) -> Array.push heap <| VMArray (t, Array.create size JavaScript.Undefined)
        | _ -> failwith "Bad array type"
        heap.Length - 1

    [<JavaScript>]
    let rec resolveMethod (cls : Dex.Class) (meth : Dex.Method) (cont : Dex.Type * Dex.MethodImpl -> unit) =
        let (Class (dtype, _, super, _, impl)) = cls
        match impl with
        | None -> failwith "Class without class_data"
        | Some (ClassImpl (_, _, _, virt)) ->
            let m = Dumbdict.tryGetWith (fun (Method (_, p1, n1)) (Method (_, p2, n2)) -> n1 = n2 && p1 = p2) virt meth
            match m with
            | Some (_, Some method_impl) -> cont (dtype, method_impl)
            | Some (_, None) -> failwith "Method not implemented" //TODO #10 throw IncompatibleClassChangeError
            | None ->
                match super with
                | None -> failwith "Method not found" //TODO #10 throw IncompatibleClassChangeError
                | Some t -> classOfType t <| fun c -> resolveMethod c meth cont

    [<JavaScript>]
    let processRequest (r : ResourceRequest, cont : ResourceReply -> unit) =
        match r with
        | RequestClass (dtype) ->
            classOfType dtype (ProvideClass >> cont)
        | ResolveMethod (refr, meth) ->
            match dereference refr with
            | VMInstance (cls, r) ->
                resolveMethod cls meth (ProvideMethod >> cont)
            | _ -> failwith "Instance expected"
        | CreateInstance dtype ->
            createInstance dtype (ProvideInstance >> cont)
        | CreateArray (size, dtype) ->
            cont << ProvideInstance <| createArray (dtype, size)
        | FillArray (refr, vals) ->
            match heap.[refr] with
            | VMArray (t, a) ->
                let n = Array.append vals (Array.sub a vals.Length (a.Length - vals.Length))
                heap.[refr] <- VMArray (t, n)
                cont RequestProcessed
            | _ -> failwith "Array expected"
        | GetArrayLength refr ->
            match heap.[refr] with
            | VMArray (_, a) ->
                cont << ProvideValue << Store.storeInt <| a.Length
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
            classStaticData dtype <| fun d -> cont << ProvideValue <| d.[f]
        | PutStaticField (f, v) ->
            let (Field (dtype, _, _)) = f
            classStaticData dtype <| fun d ->
                                        d.[f] <- v
                                        cont RequestProcessed
