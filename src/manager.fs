namespace Dalvik

module Manager =
    open System.Collections.Generic
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5
    open Coretypes
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
        let (Class (dtype, _, _, _, _, _)) = cls
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
            | Class (_, _, _, _, Some (ClassImpl (fs, _, _, _)), staticInit) ->
                let staticValues : RegValue array = [| |]
                let rec valuesFromStatic (i : int) (cont : unit -> unit) =
                    if i >= staticInit.Length then
                        cont ()
                    else
                        let st = staticInit.[i]
                        match st with
                        | StaticReg r -> Array.push staticValues r; valuesFromStatic (i+1) cont
                        | StaticString s -> (new ThreadWorker.Thread ()).CreateString s (fun str -> Array.push staticValues (RegRef str); valuesFromStatic (i+1) cont)
                        | StaticArray _ -> failwith "Static arrays are not supported" //TODO #17

                valuesFromStatic 0 <| fun () ->
                    Array.iteri (fun i -> fun v -> d.[fst <| fs.[i]] <- v) staticValues
                    Array.iter (fun f -> d.[f] <- Runtime.defaultValue f) <| Array.sub (Dumbdict.keys fs) (staticValues.Length) (fs.Length - staticValues.Length)
                    let clinit = Dex.Method (t, Proto ("V", Type "V", [| |]), "<clinit>")
                    match Runtime.getMethodImpl c true clinit with
                    | None -> cont <| (c, d)
                    | Some impl -> (new ThreadWorker.Thread ()).ExecuteMethod (t, clinit, impl) [| |] (fun () -> cont (c, d)) 
            | Class (_, _, _, _, None, _) -> cont (c, d)
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
    type VMMonitor = VMMonitor of int ref * (ThreadId * (unit -> unit)) array

    [<JavaScript>]
    let heap : (VMObject * VMMonitor) array = [| |]

    [<JavaScript>]
    let monitorable (vmobj : VMObject) : (VMObject * VMMonitor) = (vmobj, VMMonitor (ref 0, [| |]))

    [<JavaScript>]
    let dereference ref =
        let inst = heap.[ref]
        if JavaScript.TypeOf inst = JavaScript.Undefined then failwith "Bad reference" else
        inst

    [<JavaScript>]
    let createInstance (t : Dex.Type) (cont : dref -> unit) =
        classOfType t <| fun c ->
            Array.push heap << monitorable <| VMInstance (c, Dictionary ())
            cont <| heap.Length - 1

    [<JavaScript>]
    let createArray (dtype : Dex.Type, size : int) =
        match Runtime.javatypeOfType dtype with
        | JavaReferenceType (ArrayType t) -> Array.push heap << monitorable <| VMArray (t, Array.create size (Store.storeInt 0))
                                             heap.Length - 1
        | _ -> failwith "Bad array type"

    [<JavaScript>]
    let processRequest (r : ResourceRequest, w : ThreadId, cont : ResourceReply -> unit) =
        match r with
        | RequestClass (dtype) ->
            classOfType dtype (ProvideClass >> cont)
        | CreateInstance dtype ->
            createInstance dtype (ProvideInstance >> cont)
        | GetObjectType refr ->
            match heap.[refr] with
            | VMInstance (Class (t, _, _, _, _, _), _), _ ->
                cont << ProvideType <| t
            | _ -> failwith "Instance expected"
        | CreateArray (size, dtype) ->
            cont << ProvideInstance <| createArray (dtype, size)
        | FillArray (refr, vals) ->
            match heap.[refr] with
            | VMArray (t, a), mon ->
                let n = Array.append vals (Array.sub a vals.Length (a.Length - vals.Length))
                heap.[refr] <- VMArray (t, n), mon
                cont RequestProcessed
            | _ -> failwith "Array expected"
        | GetArrayLength refr ->
            match heap.[refr] with
            | VMArray (_, a), _ ->
                cont << ProvideValue << Store.storeInt <| a.Length
            | _ -> failwith "Array expected"
        | GetArrayItem (refr, i) ->
            match heap.[refr] with
            | VMArray (_, a), _ -> cont << ProvideValue <| a.[i]
            | _ -> failwith "Array expected"
        | PutArrayItem (refr, i, v) ->
            match heap.[refr] with
            | VMArray (t, a), _ ->
                a.[i] <- v
                cont RequestProcessed
            | _ -> failwith "Array expected"
        | GetInstanceField (refr, f) ->
            match heap.[refr] with
            | VMInstance (_, d), _ ->
                let v = match Dictionary.tryGet d f with
                        | Some v -> v
                        | None -> Runtime.defaultValue f
                cont << ProvideValue <| v
            | _ -> failwith "Instance expected"
        | PutInstanceField (refr, f, v) ->
            match heap.[refr] with
            | VMInstance (_, d), _ -> d.[f] <- v
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
        | GetWholeArray refr ->
            match heap.[refr] with
            | VMArray (_, data), _ -> cont << ProvideArray <| data
            | _ -> failwith "GetWholeArray of object"

        | EnterMonitor dref ->
            let _, VMMonitor (mcount, mqueue) = heap.[dref]
            if Array.length mqueue > 0 && ThreadId.JsCompare(fst mqueue.[0], w) then
                mcount := !mcount + 1
                cont RequestProcessed
            else
                Array.push mqueue (w, fun () -> cont RequestProcessed)
                if Array.length mqueue = 1 then
                    cont RequestProcessed
        | ExitMonitor dref ->
            let _, VMMonitor (mcount, mqueue) = heap.[dref]
            if not (ThreadId.JsCompare (fst mqueue.[0], w)) then failwith "Thread does not own this monitor" else
            if !mcount > 0 then
                mcount := !mcount - 1
                cont RequestProcessed
            else
                Array.shift mqueue |> ignore
                cont RequestProcessed
                if Array.length mqueue > 0 then
                    (snd mqueue.[0]) ()
