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
        let mutable ret : RegValue option = None
        member private this.PushMethod (meth : Dex.Method) (args : RegValue array) =
            let frame = new ThreadFrame (this, meth, args)
            Array.push frames <| frame
            frame
        member this.ExecuteMethod (meth : Dex.Method) (args : RegValue array) (cont : unit -> unit) =
            (this.PushMethod meth args).Interpret 0 cont
        member this.Return (r : RegValue option) (cont : unit -> unit) =
            ret <- r
            Array.pop frames
            cont ()
        member this.LastResult = match ret with
                                 | Some r ->
                                    ret <- None
                                    r
                                 | None -> failwith "Can't 'move-result', no result"
        member this.Start (meth : Dex.Method, args : RegValue array) = this.ExecuteMethod meth args this.Finish
        member this.Finish () = ()
    and 
     [<JavaScript>]
        ThreadFrame (thread : Thread, meth : Dex.Method, args : RegValue array) =
        let regs = Array.append (Array.create (int meth.registers_size - args.Length) JavaScript.Undefined) args
        let thisclass = meth.dclass

        member this.Thread () = thread
        member this.GetReg (i : reg) = regs.[int i]
        member this.SetReg (i : reg, v : RegValue) = regs.[int i] <- v
    
        member this.Interpret (i : int) (cont : unit -> unit) =
            if int i >= meth.insns.Length then cont () else
            let goto t = this.Interpret t cont
            let next () = goto (i + 1)
            let ins = meth.insns.[i]
            match ins with
                | Nop () -> next ()

                | Move (r1, r2) -> this.SetReg(r1, this.GetReg r2); next ()
                | MoveResult r -> this.SetReg(r, thread.LastResult); next ()
                //| MoveException //TODO #10

                | ReturnVoid () -> thread.Return None cont
                | Return r -> thread.Return (Some <| this.GetReg r) cont

                | Const4 (r, v) -> this.SetReg (r, Store.storeInt << int32 <| v); next ()
                | Const16 (r, v) -> this.SetReg (r, Store.storeInt << int32 <| v); next ()
                | Const (r, v) -> this.SetReg (r, Store.storeInt <| v); next ()
                | ConstHigh16 (r, v) -> this.SetReg (r, Store.storeInt (int32 v <<< 16)); next ()
                | ConstWide16 (r, v) -> this.SetReg (r, Store.storeLong << GLong.FromInt << int32 <| v); next ()
                | ConstWide32 (r, v) -> this.SetReg (r, Store.storeLong << GLong.FromInt <| v); next ()
                | ConstWide (r, v) -> this.SetReg (r, Store.storeLong v); next ()
                | ConstWideHigh16 (r, v) -> this.SetReg (r, Store.storeLong <| GLong.FromBits (0, int32 v <<< 16)); next ()
                | ConstString (r, p) -> this.SetReg (r, RegAny << As<obj> <| p); next ()
                //| ConstClass (r, p) -> this.SetReg (r, JsRef // TODO: get a dalvik-reference to a string? Or request and store the string itself?

                //| MonitorEnter r -> this.GetReg r // TODO: send monitor-enter message
                //| MonitorExit r -> this.GetReg r // TODO: send monitor-enter message

                // ops missing…

                | NewInstance (r, t) -> createInstance t (fun dref ->
                                                            this.SetReg (r, RegRef dref)
                                                            next ())

                // ops missing…

                | Goto offset -> match offset with
                                 | AbsoluteIndex i -> goto i
                                 | RelativeBytes _ -> failwith "Unresolved goto offset"

                // ops missing…

                | If (kind, (a, b, offset)) ->
                    let i = match offset with
                            | AbsoluteIndex i -> i
                            | RelativeBytes _ -> failwith "Unresolved offset"
                    let va = Store.loadInt (this.GetReg a)
                    let vb = Store.loadInt (this.GetReg b)
                    let jump = match kind with
                               | Eq -> va = vb
                               | Ne -> va <> vb
                               | Lt -> va < vb
                               | Ge -> va >= vb
                               | Gt -> va > vb
                               | Le -> va <= vb
                    if jump then goto i else next ()

                | IfZ (kind, (a, offset)) ->
                    let i = match offset with
                            | AbsoluteIndex i -> i
                            | RelativeBytes _ -> failwith "Unresolved offset"
                    let va = Store.loadInt (this.GetReg a)
                    let jump = match kind with
                               | Eq -> va = 0
                               | Ne -> va <> 0
                               | Lt -> va < 0
                               | Ge -> va >= 0
                               | Gt -> va > 0
                               | Le -> va <= 0
                    if jump then goto i else next ()

                // ops missing…

                | Invoke (kind, (count, meth, a1, a2, a3, a4, a5)) ->
                    let args = Array.map this.GetReg <| Array.sub [| a1; a2; a3; a4; a5 |] 0 (int count)
                    match kind with
                    | InvokeVirtual
                    | InvokeInterface ->
                        let o = match this.GetReg a1 with
                                | RegRef dref -> dref
                                | _ -> failwith "Invoking a method on non-object"
                        getVirtualMethod (o, meth.name, meth.proto) (fun m -> thread.ExecuteMethod m args next)
                    | InvokeDirect ->
                        thread.ExecuteMethod meth args next
                    // TODO: static and super

                // ops missing…

                | AddInt (d, a, b) ->
                    this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) + Store.loadInt (this.GetReg b))); next ()

    let mutable thread = None
    let init (meth : Dex.Method,  args : RegValue array) =
        let t = new Thread ()
        thread <- Some t
        t.ExecuteMethod meth args (fun () -> ())