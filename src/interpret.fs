namespace Dalvik

module ThreadWorker =
    open System.Collections.Generic
    open IntelliFactory.WebSharper
    open Shared
    open Dex

    // All kinds of communication with shared resources manager

    [<Stub>]
    let requestResource (r : ResourceRequest, cont : ResourceReply -> unit) : unit = X<_>

    [<JavaScript>]
    let cacheClasses : Dictionary<Dex.Type, Dex.Class> = Dictionary ()

    [<JavaScript>]
    let getClass (dtype : Dex.Type, cont : Dex.Class -> unit) =
        match Dictionary.tryGet cacheClasses dtype with
        | Some c -> cont c
        | None ->
            requestResource (RequestClass dtype, fun r ->
                match r with
                    | ProvideClass c ->
                        cacheClasses.Add (dtype, c)
                        cont c
                    | _ -> failwith <| "Unexpected reply. I need a Class but got a "  + r.ToString ())

    [<JavaScript>]
    let getVirtualMethod (refr : dref, name : string, proto : Proto) (cont : Dex.Method -> unit) =
        requestResource (ResolveMethod (refr, name, proto), fun r ->
            match r with
                | ProvideMethod m -> cont m
                | _ -> failwith <| "Unexpected reply. I need a Method but got a " + r.ToString ())

    [<JavaScript>]
    let createInstance (dtype : Dex.Type) (cont : dref -> unit) =
        requestResource (CreateInstance dtype, fun r ->
            match r with
                | ProvideInstance r -> cont r
                | _ -> failwith <| "Unexpected reply. I need an Instance but got a " + r.ToString ())


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
        let regs = Array.append (Array.create (int meth.registers_size - args.Length) (JsRef null)) args

        member this.Thread () = thread
        member this.GetReg (i : reg) = regs.[int i]
        member this.SetReg (i : reg, v : JsValue) = regs.[int i] <- v
    
        member this.Interpret (i : uint16) (cont : unit -> unit) =
            if int i >= meth.insns.Length then cont () else
            let goto t = this.Interpret t cont
            let next () = goto (i + 1us)
            let ins = meth.insns.[int i]
            match ins with
                | Nop () -> next ()

                | Move (r1, r2) -> this.SetReg(r1, this.GetReg r2); next ()
                | MoveResult r -> this.SetReg(r, thread.LastResult); next ()
                //| MoveException //TODO #10

                | ReturnVoid () -> thread.Return <| None
                | Return r -> thread.Return <| Some (this.GetReg r)

                | Const4 (r, v) -> this.SetReg (r, Store.storeInt << int32 <| v); next ()
                | Const16 (r, v) -> this.SetReg (r, Store.storeInt << int32 <| v); next ()
                | Const (r, v) -> this.SetReg (r, Store.storeInt <| v); next ()
                | ConstHigh16 (r, v) -> this.SetReg (r, Store.storeInt (int32 v <<< 16)); next ()
                | ConstWide16 (r, v) -> this.SetReg (r, Store.storeLong << GLong.FromInt << int32 <| v); next ()
                | ConstWide32 (r, v) -> this.SetReg (r, Store.storeLong << GLong.FromInt <| v); next ()
                | ConstWide (r, v) -> this.SetReg (r, Store.storeLong v)
                | ConstWideHigh16 (r, v) -> this.SetReg (r, Store.storeLong <| GLong.FromBits (0, int32 v <<< 16))
                //| ConstString (r, p) -> this.SetReg (r, JsRef // TODO: get a dalvik-reference to a string? Or request and store the string itself?
                //| ConstStringJumbo (r, p) -> this.SetReg (r, JsRef // TODO: get a dalvik-reference to a string? Or request and store the string itself?
                //| ConstClass (r, p) -> this.SetReg (r, JsRef // TODO: get a dalvik-reference to a string? Or request and store the string itself?

                //| MonitorEnter r -> this.GetReg r // TODO: send monitor-enter message
                //| MonitorExit r -> this.GetReg r // TODO: send monitor-enter message