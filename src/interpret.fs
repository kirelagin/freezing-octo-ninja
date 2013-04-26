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
    let getClass (dtype : Dex.Type) (cont : Dex.Class -> unit) =
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
    let getMethodImpl (meth : Dex.Method) (direct : bool) (cont : Dex.MethodImpl -> unit) =
        let (Method (dtype, _, _)) = meth
        getClass dtype <| fun c ->
                            match Runtime.getMethodImpl c direct meth with
                            | None -> failwith "Method not found"
                            | Some impl -> cont impl

    [<JavaScript>]
    let getVirtualMethod (refr : dref, meth : Dex.Method) (cont : Dex.Type * Dex.MethodImpl -> unit) =
        requestResource (ResolveMethod (refr, meth), fun r ->
            match r with
                | ProvideMethod (t, m) -> cont (t, m)
                | _ -> failwith <| "Unexpected reply. I need a Method but got a " + r.ToString ())


    [<JavaScript>]
    let createInstance (dtype : Dex.Type) (cont : dref -> unit) =
        requestResource (CreateInstance dtype, fun r ->
            match r with
                | ProvideInstance r -> cont r
                | _ -> failwith <| "Unexpected reply. I need an Instance but got a " + r.ToString ())

    [<JavaScript>]
    let createArray (size : int32, dtype : Dex.Type) (cont : dref -> unit) =
        requestResource (CreateArray (size, dtype), fun r ->
            match r with
            | ProvideInstance r -> cont r
            | _ -> failwith <| "Unexpected reply. I need an Instance but got a " + r.ToString ())

    [<JavaScript>]
    let fillArray (refr : dref, vals : RegValue array) (cont : unit -> unit) =
        requestResource (FillArray (refr, vals), fun r ->
            match r with
            | RequestProcessed -> cont ()
            | _ -> failwith <| "Unexpected reply. I need a RequestProcessed but got a " + r.ToString ())

    [<JavaScript>]
    let getArrayLength (refr : dref) (cont : RegValue -> unit) =
        requestResource (GetArrayLength refr, fun r ->
            match r with
            | ProvideValue v -> cont v
            | _ -> failwith <| "Unexpected reply. I need a value but got a " + r.ToString ())

    [<JavaScript>]
    let arrayGet (refr : dref) (i : int32) (cont : RegValue -> unit) =
        requestResource (GetArrayItem (refr, i), fun r ->
            match r with
            | ProvideValue v -> cont v
            | _ -> failwith <| "Unexpected reply. I need a value but got a " + r.ToString ())

    [<JavaScript>]
    let arrayPut (refr : dref) (i : int32) (v : RegValue) (cont : unit -> unit) =
        requestResource (PutArrayItem (refr, i, v), fun r ->
            match r with
            | RequestProcessed -> cont ()
            | _ -> failwith <| "Unexpected reply. I need a RequestProcessed but got a " + r.ToString ())

    [<JavaScript>]
    let staticGet (field : Dex.Field) (cont : RegValue -> unit) =
        requestResource (GetStaticField field, fun r ->
            match r with
            | ProvideValue v -> cont v
            | _ -> failwith <| "Unexpected reply. I need a value but got a " + r.ToString ())

    [<JavaScript>]
    let staticPut (field : Dex.Field) (v : RegValue) (cont : unit -> unit) =
        requestResource (PutStaticField (field, v), fun r ->
            match r with
            | RequestProcessed -> cont ()
            | _ -> failwith <| "Unexpected reply. I need a RequestProcessed but got a " + r.ToString ())

    [<JavaScript>]
    let instanceGet (refr : dref) (field : Dex.Field) (cont : RegValue -> unit) =
        requestResource (GetInstanceField (refr, field), fun r ->
            match r with
            | ProvideValue v -> cont v
            | _ -> failwith <| "Unexpected reply. I need a value but got a " + r.ToString ())

    [<JavaScript>]
    let instancePut (refr : dref) (field : Dex.Field) (v : RegValue) (cont : unit -> unit) =
        requestResource (PutInstanceField (refr, field, v), fun r ->
            match r with
            | RequestProcessed -> cont ()
            | _ -> failwith <| "Unexpected reply. I need a RequestProcessed but got a " + r.ToString ())

    [<JavaScript>]
    type Thread () =
        let frames : ThreadFrame array = [| |]
        let mutable ret : RegValue option = None
        member this.ExecuteMethod (mclass : Dex.Type, meth : Dex.MethodImpl) (args : RegValue array) (cont : unit -> unit) =
            match meth with
            | MethodImpl (registers_size, ins_size, outs_size, insns) ->
                let frame = new ThreadFrame (this, mclass, insns, int registers_size, args)
                Array.push frames <| frame
                frame.Interpret 0 cont
        member this.Return (r : RegValue option) (cont : unit -> unit) =
            ret <- r
            Array.pop frames
            cont ()
        member this.LastResult = match ret with
                                 | Some r ->
                                    ret <- None
                                    r
                                 | None -> failwith "Can't 'move-result', no result"
        member this.Start (dtype : Dex.Type, meth : Dex.MethodImpl) (args : RegValue array) = this.ExecuteMethod (dtype, meth) args this.Finish
        member this.Finish () = ()
    and 
     [<JavaScript>]
        ThreadFrame (thread : Thread, thisclass : Type, insns : Instruction array, registers_size : int, args : RegValue array) =
        let regs = Array.append (Array.create (registers_size - args.Length) (Store.storeInt 0)) args

        member this.Thread () = thread
        member this.GetReg (i : reg) = regs.[int i]
        member this.SetReg (i : reg, v : RegValue) = regs.[int i] <- v
    
        member this.Interpret (i : int) (cont : unit -> unit) =
            if int i >= insns.Length then cont () else
            let goto t = this.Interpret t cont
            let next () = goto (i + 1)
            let ins = insns.[i]
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
                | ConstString (r, s) -> createArray (s.Length, Dex.Type "C") (fun aref ->
                                            fillArray (aref, Array.map Store.storeInt << String.toIntArray <| s) (fun () ->
                                                let strtype = Dex.Type "Ljava/lang/String;"
                                                createInstance strtype (fun str ->
                                                    let proto = Dex.Proto ("VIIL", Dex.Type "V", [| Dex.Type "I"; Dex.Type "I"; Dex.Type "[C" |])
                                                    let meth = Dex.Method (strtype, proto, "<init>")
                                                    getMethodImpl meth true (fun m ->
                                                        thread.ExecuteMethod (strtype, m) [| RegRef str; Store.storeInt 0; Store.storeInt s.Length; RegRef aref|] next))))
                //| ConstClass (r, p) -> this.SetReg (r, JsRef // TODO: get a dalvik-reference to a string? Or request and store the string itself?

                //| MonitorEnter r -> this.GetReg r // TODO: send monitor-enter message
                //| MonitorExit r -> this.GetReg r // TODO: send monitor-enter message

                // ops missing…

                | ArrayLength (d, r) ->
                    match this.GetReg r with
                    | RegRef dref -> getArrayLength dref (fun l -> this.SetReg (d, l); next ())
                    | _ -> failwith "array-length on non-object"

                | NewInstance (r, t) -> createInstance t (fun dref ->
                                                            this.SetReg (r, RegRef dref)
                                                            next ())

                | NewArray (d, s, t) -> createArray (Store.loadInt << this.GetReg <| s, t) (fun dref ->
                                                                                                this.SetReg (d, RegRef dref)
                                                                                                next ())

                | FilledNewArray _ -> failwith "Not implemented"
                | FilledNewArrayRange _ -> failwith "Not implemented"

                | FillArrayData (r, vals) ->
                    match this.GetReg r with
                    | RegRef refr -> fillArray (refr, vals) next
                    | _ -> failwith "Bad destination register"

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
                    let jump = match this.GetReg a with
                               | RegRef _ ->
                                    match kind with
                                    | Eq -> false
                                    | Ne -> true
                                    | _ -> failwith "Invalid comparison of reference value with zero"
                               | _ ->
                                    let va = Store.loadInt (this.GetReg a)
                                    match kind with
                                               | Eq -> va = 0
                                               | Ne -> va <> 0
                                               | Lt -> va < 0
                                               | Ge -> va >= 0
                                               | Gt -> va > 0
                                               | Le -> va <= 0
                    if jump then goto i else next ()

                | Aget (d, r, i) ->
                    match this.GetReg r with
                    | RegRef refr -> arrayGet refr (Store.loadInt << this.GetReg <| i) (fun v -> this.SetReg (d, v); next ())
                    | _ -> failwith "aget on non-object"
                | Aput (s, r, i) ->
                    match this.GetReg r with
                    | RegRef refr -> arrayPut refr (Store.loadInt << this.GetReg <| i) (this.GetReg s) next
                    | _ -> failwith "aput on non-object"

                | Iget (d, r, f) ->
                    match this.GetReg r with
                    | RegRef refr -> instanceGet refr f (fun v -> this.SetReg (d, v); next ())
                    | _ -> failwith "iget on non-object"
                | Iput (s, r, f) ->
                    match this.GetReg r with
                    | RegRef refr -> instancePut refr f (this.GetReg s) next
                    | _ -> failwith "iput on non-object"

                | Sget (d, f) -> staticGet f (fun v -> this.SetReg(d, v); next ())
                | Sput (s, f) -> staticPut f (this.GetReg s) next

                | Invoke (kind, (count, meth, a1, a2, a3, a4, a5)) ->
                    let args = Array.map this.GetReg <| Array.sub [| a1; a2; a3; a4; a5 |] 0 (int count)
                    match kind with
                    | InvokeVirtual
                    | InvokeInterface ->
                        let o = match this.GetReg a1 with
                                | RegRef dref -> dref
                                | _ -> failwith "Invoking a method on non-object"
                        getVirtualMethod (o, meth) (fun m -> thread.ExecuteMethod m args next)
                    | InvokeDirect ->
                        let (Method (dtype, _, _)) = meth
                        getMethodImpl meth true (fun m -> thread.ExecuteMethod (dtype, m) args next)
                    // TODO: static and super

                // ops missing…

                | IntToSmall (_, (d, s)) -> this.SetReg (d, this.GetReg s); next ()

                | AddInt (d, a, b) ->
                    this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) + Store.loadInt (this.GetReg b))); next ()

                // ops missing…

                | AddIntLit (d, a, i) ->
                    this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) + i)); next ()

                // ops missing…

    [<JavaScript>]
    let mutable thread = None

    [<JavaScript>]
    let init ((dtype : Dex.Type, meth : Dex.MethodImpl),  args : RegValue array) =
        let t = new Thread ()
        thread <- Some t
        t.Start (dtype, meth) args
