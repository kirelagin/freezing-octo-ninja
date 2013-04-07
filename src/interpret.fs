namespace Dalvik

module ThreadWorker =
    open IntelliFactory.WebSharper
    open Shared
    open ByteCode

    // All kinds of communication with shared resources manager

    [<Stub>]
    let requestResource (r : ResourceRequest, cont : ResourceReply -> unit) : unit = X<_>

    [<JavaScript>]
    let cacheTypes : Dex.Type array = [| |]

    [<JavaScript>]
    let getType (idx : uint16) (cont : Dex.Type -> unit) =
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
    type Thread () =
        let frames : ThreadFrame array = [| |]
        let mutable ret : JsValue option = None
        member this.PushMethod (meth : Dex.Method, args : JsValue array) = Array.push frames <| new ThreadFrame (this, meth, args)
        member this.Return (r : JsValue option) = ret <- r
        member this.LastResult = match ret with | Some r -> r | None -> failwith "Can't 'move-result', no result"
    and 
     [<JavaScript>]
        ThreadFrame (thread : Thread, meth : Dex.Method, args : JsValue array) =
        let regs = Array.append (Array.create (int meth.RegistersSize - args.Length) (JsRef null)) args

        member this.Thread () = thread
        member this.GetReg (i : reg) = regs.[int i]
        member this.SetReg (i : reg, v : JsValue) = regs.[int i] <- v
    
        member this.Interpret (i : uint16) (cont : unit -> unit) =
            if int i >= meth.Insns.Length then cont () else
            let next () = this.Interpret (i + 1us) cont
            let ins = meth.Insns.[int i]
            match ins with
                | Nop () -> next ()
                | Move (r1, r2) -> this.SetReg(r1, this.GetReg r2); next ()
                | MoveResult r -> this.SetReg(r, thread.LastResult); next ()
                //| MoveException //TODO #10
                | ReturnVoid () -> thread.Return <| None
                | Return r -> thread.Return <| Some (this.GetReg r)
                | Const4 (r, v) -> this.SetReg (r, Js32 << int32 <| v); next ()
                | Const16 (r, v) -> this.SetReg (r, Js32 << int32 <| v); next ()
                | Const (r, v) -> this.SetReg (r, Js32 <| v); next ()
                | ConstHigh16 (r, v) -> this.SetReg (r, Js32 (int32 v <<< 16)); next ()