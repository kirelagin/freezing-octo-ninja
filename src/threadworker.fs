namespace Dalvik

module ThreadWorker =
    open IntelliFactory.WebSharper
    open Shared

    // All kinds of communication with shared resources manager

    [<Stub>]
    let requestResource (r : ResourceRequest, cont : ResourceReply -> unit) : unit = X<_>

    [<JavaScript>]
    let cacheTypes : Dex.Type array = [| |]

    [<JavaScript>]
    let getType (idx : uint16, cont : Dex.Type -> unit) =
        if JavaScript.TypeOf cacheTypes.[int idx] = JavaScript.Kind.Undefined then // TODO: int convertion sucks. Use hashmap?
            requestResource (RequestType idx, fun r ->
                match r with
                    | ProvideType t ->
                        cacheTypes.[int idx] <- t // TODO: int convertion sucks
                        cont t
                    | _ -> failwith <| "Unexpected reply. I need a Type but got a " + r.ToString ())
        else
            cont <| cacheTypes.[int idx] // TODO: int convertion sucks

    [<JavaScript>]
    let cacheClasses : ObjectMap.ObjectMap<Dex.Type, Dex.Class> = ObjectMap.empty

    [<JavaScript>]
    let getClass (descriptor : Dex.Type, cont : Dex.Class -> unit) =
        match ObjectMap.lookup cacheClasses descriptor with
            | None -> requestResource (RequestClass descriptor, fun r ->
                        match r with
                            | ProvideClass c ->
                                ObjectMap.update cacheClasses descriptor c
                                cont c
                            | _ -> failwith <| "Unexpected reply. I need a Class but got a "  + r.ToString ())
            | Some c -> cont c


    [<JavaScript>]
    type ThreadFrame (meth : Dex.Method, args : JsValue array) =
        let regs = Array.append (Array.create (int meth.RegistersSize - args.Length) (JsRef null)) args
        let mutable ret : JsValue option = None

        member this.Return (r : JsValue) = ret <- Some r
        member this.GetReg (i : uint16) = regs.[int i]
        member this.SetReg (i: uint16, v : JsValue) = regs.[int i] <- v

    [<JavaScript>]
    let frames : ThreadFrame array = [| |]