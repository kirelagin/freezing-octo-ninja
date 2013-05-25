namespace Dalvik

module ThreadWorker =
    open System.Collections.Generic
    open IntelliFactory.WebSharper
    open Shared
    open Coretypes
    open Dex

    // All kinds of communication with shared resources manager

    [<Stub>]
    let requestResource (r : ResourceRequest, cont : ResourceReply -> unit) : unit = X<_>

    [<Stub>]
    let requestInteraction (r : InteractionRequest, cont : unit -> unit) : unit = X<_>

    [<JavaScript>]
    let cacheClasses : Dictionary<Dex.Type, Dex.Class> = Dictionary ()

    [<JavaScript>]
    let getObjectType (refr : dref) (cont : Dex.Type -> unit) =
        requestResource (GetObjectType refr, fun r ->
            match r with
            | ProvideType t -> cont t
            | _ -> failwith <| "Unexpected reply. I need a type but got a " + r.ToString ())

    [<JavaScript>]
    let getClass (dtype : Dex.Type) (cont : Dex.Class -> unit) =
        match Dictionary.tryGet cacheClasses dtype with
        | Some c -> cont c
        | None ->
            requestResource (RequestClass dtype, fun r ->
                match r with
                    | ProvideClass c ->
                        cacheClasses.[dtype] <- c
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
    let rec resolveMethod (t : Dex.Type) (meth : Dex.Method) (cont : Dex.Type * Dex.Method * Dex.MethodImpl -> unit) =
        getClass t <| fun cls ->
            let (Class (_, _, super, _, impl, _)) = cls
            match impl with
            | None -> failwith "Class without class_data"
            | Some (ClassImpl (_, _, _, virt)) ->
                let m = Dumbdict.tryGetWith (fun (Method (_, p1, n1)) (Method (_, p2, n2)) -> n1 = n2 && p1 = p2) virt meth
                match m with
                | Some (_, Some method_impl) -> cont (t, meth, method_impl)
                | Some (_, None) -> failwith "Method not implemented" //TODO #10 throw IncompatibleClassChangeError
                | None ->
                    match super with
                    | None -> failwith "Method not found" //TODO #10 throw IncompatibleClassChangeError
                    | Some t -> resolveMethod t meth cont

    [<JavaScript>]
    let getWholeArray (refr : dref) (cont : RegValue array -> unit) =
        requestResource (GetWholeArray refr, fun r ->
            match r with
            | ProvideArray data -> cont data
            | _ -> failwith <| "Unexpected reply. I need an array but got a " + r.ToString ())


    // Native methods library
    [<JavaScript>]
    let nativelib : Dictionary<Dex.Method, (RegValue array * (RegValue option -> unit) -> unit)> = Dictionary ()

    [<JavaScript>]
    type Thread () =
        let frames : ThreadFrame array = [| |]
        let mutable ret : RegValue option = None
        member this.ExecuteMethod (mclass : Dex.Type, meth : Dex.Method, impl : Dex.MethodImpl) (args : RegValue array) (cont : unit -> unit) =
            match impl with
            | MethodImpl (registers_size, ins_size, outs_size, insns) ->
                let frame = new ThreadFrame (this, meth, mclass, insns, int registers_size, args)
                Array.push frames <| frame
                frame.Interpret cont
            | NativeMethod ->
                nativelib.[meth] (args, fun r -> ret <- r; cont ())
        member this.Return (r : RegValue option) (cont : unit -> unit) =
            ret <- r
            Array.pop frames
            cont ()
        member this.LastResult = match ret with
                                 | Some r ->
                                    ret <- None
                                    r
                                 | None -> failwith "Can't 'move-result', no result"
        member this.Start (dtype : Dex.Type, meth : Dex.Method, impl : Dex.MethodImpl) (args : RegValue array) = this.ExecuteMethod (dtype, meth, impl) args this.Finish
        member this.Finish () = ()

        member this.CreateString (s : string) (cont : dref -> unit) =
            createArray (s.Length, Dex.Type "[C") (fun aref ->
                fillArray (aref, Array.map Store.storeInt << String.toIntArray <| s) (fun () ->
                    let strtype = Dex.Type "Ljava/lang/String;"
                    createInstance strtype (fun str ->
                        let proto = Dex.Proto ("VIIL", Dex.Type "V", [| Dex.Type "I"; Dex.Type "I"; Dex.Type "[C" |])
                        let meth = Dex.Method (strtype, proto, "<init>")
                        getMethodImpl meth true (fun m ->
                            this.ExecuteMethod (strtype, meth, m) [| RegRef str; Store.storeInt 0; Store.storeInt s.Length; RegRef aref|] (fun () -> cont str)))))

    and 
     [<JavaScript>]
        ThreadFrame (thread : Thread, meth : Method, thisclass : Type, insns : Instruction array, registers_size : int, args : RegValue array) =
        let regs = Array.append (Array.create (registers_size - args.Length) (Store.storeInt 0)) args
        let mutable ip = 0

        member this.Thread () = thread
        member this.GetReg (i : reg) = regs.[int i]
        member this.SetReg (i : reg, v : RegValue) = regs.[int i] <- v
    
        member this.Interpret (cont : unit -> unit) =
            let unwind = ref false

            let goto t = ip <- t
            let next () = goto (ip + 1)
                          if !unwind then
                              this.Interpret cont
                          else
                              () 
            while not !unwind do
                match insns.[ip] with
                    | Nop () -> next ()

                    | Move (r1, r2) -> this.SetReg(r1, this.GetReg r2); next ()
                    | MoveResult r -> this.SetReg(r, thread.LastResult); next ()
                    //| MoveException //TODO #10

                    | ReturnVoid () -> unwind := true; thread.Return None cont
                    | Return r -> unwind := true; thread.Return (Some <| this.GetReg r) cont

                    | Const4 (r, v) -> this.SetReg (r, Store.storeInt << int32 <| v); next ()
                    | Const16 (r, v) -> this.SetReg (r, Store.storeInt << int32 <| v); next ()
                    | Const (r, v) -> this.SetReg (r, Store.storeInt <| v); next ()
                    | ConstHigh16 (r, v) -> this.SetReg (r, Store.storeInt (int32 v <<< 16)); next ()
                    | ConstWide16 (r, v) -> this.SetReg (r, Store.storeLong << GLong.FromInt << int32 <| v); next ()
                    | ConstWide32 (r, v) -> this.SetReg (r, Store.storeLong << GLong.FromInt <| v); next ()
                    | ConstWide (r, v) -> this.SetReg (r, Store.storeLong << GLong.FromBits <| v); next ()
                    | ConstWideHigh16 (r, v) -> this.SetReg (r, Store.storeLong <| GLong.FromBits (0, int32 v <<< 16)); next ()
                    | ConstString (r, s) -> unwind := true; thread.CreateString s (fun str -> this.SetReg (r, RegRef str); next ())
                    //| ConstClass (r, p) -> this.SetReg (r, JsRef // TODO: get a dalvik-reference to a string? Or request and store the string itself?

                    //| MonitorEnter r -> this.GetReg r // TODO: send monitor-enter message
                    //| MonitorExit r -> this.GetReg r // TODO: send monitor-enter message

                    // ops missing…

                    | ArrayLength (d, r) ->
                        unwind := true
                        match this.GetReg r with
                        | RegRef dref -> getArrayLength dref (fun l -> this.SetReg (d, l); next ())
                        | _ -> failwith "array-length on non-object"

                    | NewInstance (r, t) -> unwind := true
                                            createInstance t (fun dref ->
                                                                this.SetReg (r, RegRef dref)
                                                                next ())

                    | NewArray (d, s, t) -> unwind := true
                                            createArray (Store.loadInt << this.GetReg <| s, t) (fun dref ->
                                                                                                    this.SetReg (d, RegRef dref)
                                                                                                    next ())

                    | FilledNewArray _ -> failwith "Not implemented"
                    | FilledNewArrayRange _ -> failwith "Not implemented"

                    | FillArrayData (r, vals) ->
                        unwind := true
                        match this.GetReg r with
                        | RegRef refr -> fillArray (refr, vals) next
                        | _ -> failwith "Bad destination register"

                    // ops missing…

                    | Goto offset -> match offset with
                                     | AbsoluteIndex i -> goto i
                                     | RelativeBytes _ -> failwith "Unresolved goto offset"
                    | Switch (r, cases) ->
                        let i = Store.loadInt (this.GetReg r)
                        match Dumbdict.tryGet cases i with
                        | Some offset -> match offset with
                                         | AbsoluteIndex i -> goto i
                                         | RelativeBytes _ -> failwith "Unresolved switch offset"
                        | None -> next ()

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
                        unwind := true
                        match this.GetReg r with
                        | RegRef refr -> arrayGet refr (Store.loadInt << this.GetReg <| i) (fun v -> this.SetReg (d, v); next ())
                        | _ -> failwith "aget on non-object"
                    | Aput (s, r, i) ->
                        unwind := true
                        match this.GetReg r with
                        | RegRef refr -> arrayPut refr (Store.loadInt << this.GetReg <| i) (this.GetReg s) next
                        | _ -> failwith "aput on non-object"

                    | Iget (d, r, f) ->
                        unwind := true
                        match this.GetReg r with
                        | RegRef refr -> instanceGet refr f (fun v -> this.SetReg (d, v); next ())
                        | _ -> failwith "iget on non-object"
                    | Iput (s, r, f) ->
                        unwind := true
                        match this.GetReg r with
                        | RegRef refr -> instancePut refr f (this.GetReg s) next
                        | _ -> failwith "iput on non-object"

                    | Sget (d, f) -> unwind := true; staticGet f (fun v -> this.SetReg(d, v); next ())
                    | Sput (s, f) -> unwind := true; staticPut f (this.GetReg s) next

                    | Invoke (kind, (count, meth, a1, a2, a3, a4, a5)) ->
                        unwind := true
                        let args = Array.map this.GetReg <| Array.sub [| a1; a2; a3; a4; a5 |] 0 (int count)
                        match kind with
                        | InvokeVirtual
                        | InvokeInterface ->
                            match this.GetReg a1 with
                            | RegRef r ->
                                getObjectType r <| fun t ->
                                    resolveMethod t meth (fun x -> thread.ExecuteMethod x args next)
                            | _ -> failwith "invoke-interface on non-object"
                        | InvokeDirect
                        | InvokeStatic ->
                            let (Method (dtype, _, _)) = meth
                            getMethodImpl meth true (fun m -> thread.ExecuteMethod (dtype, meth, m) args next)
                        | InvokeSuper ->
                            match this.GetReg a1 with
                            | RegRef r ->
                                getObjectType r <| fun t ->
                                    getClass t <| fun (Class (_, _, s, _, _, _)) ->
                                        match s with
                                        | Some supert -> resolveMethod supert meth (fun x -> thread.ExecuteMethod x args next)
                                        | None -> failwith "invoke-supert on class that doesn't have a super"
                            | _ -> failwith "invoke-super on non-object"

                    | Neg (t, (d, a)) ->
                        let v = this.GetReg a
                        let newval =
                            match t with
                            | CoreInteger IntegerInt -> Store.storeInt <| -Store.loadInt v
                            | CoreInteger IntegerLong -> Store.storeLong <| (Store.loadLong v).Negate ()
                            | CoreFloating FloatingFloat -> Store.storeFloat <| -Store.loadFloat v
                            | CoreFloating FloatingDouble -> Store.storeDouble <| -Store.loadDouble v
                        this.SetReg(d, newval)
                        next ()
                    | Not (t, (d, a)) ->
                        let v = this.GetReg a
                        let newval =
                            match t with
                            | IntegerInt -> Store.storeInt <| ~~~(Store.loadInt v)
                            | IntegerLong -> Store.storeLong <| (Store.loadLong v).Not ()
                        this.SetReg(d, newval)
                        next ()

                    | Convert ((t1, t2), (d, s)) ->
                        (match (t1, t2) with
                                         | (CoreInteger IntegerInt, CoreInteger IntegerLong) -> Store.storeLong << Convert.intToLong << Store.loadInt
                                         | (CoreInteger IntegerInt, CoreFloating FloatingFloat) -> Store.storeFloat << Convert.intToFloat << Store.loadInt
                                         | (CoreInteger IntegerInt, CoreFloating FloatingDouble) -> Store.storeDouble << Convert.intToDouble << Store.loadInt
                                         | (CoreInteger IntegerLong, CoreInteger IntegerInt) -> Store.storeInt << Convert.longToInt << Store.loadLong
                                         | (CoreInteger IntegerLong, CoreFloating FloatingFloat) -> Store.storeFloat << Convert.longToFloat << Store.loadLong
                                         | (CoreInteger IntegerLong, CoreFloating FloatingDouble) -> Store.storeDouble << Convert.longToDouble << Store.loadLong
                                         | (CoreFloating FloatingFloat, CoreInteger IntegerInt) -> Store.storeInt << Convert.floatToInt << Store.loadFloat
                                         | (CoreFloating FloatingFloat, CoreInteger IntegerLong) -> Store.storeLong << Convert.floatToLong << Store.loadFloat
                                         | (CoreFloating FloatingFloat, CoreFloating FloatingDouble) -> Store.storeDouble << Convert.floatToDouble << Store.loadFloat
                                         | (CoreFloating FloatingDouble, CoreInteger IntegerInt) -> Store.storeInt << Convert.doubleToInt << Store.loadDouble
                                         | (CoreFloating FloatingDouble, CoreInteger IntegerLong) -> Store.storeLong << Convert.doubleToLong << Store.loadDouble
                                         | (CoreFloating FloatingDouble, CoreFloating FloatingFloat) -> Store.storeFloat << Convert.doubleToFloat << Store.loadDouble
                                         | _ -> failwith "Invalid convertion"
                        ) (this.GetReg s) |> curry this.SetReg d
                        next ()
                    | IntToSmall (t, (d, s)) ->
                        let i = Store.loadInt <| this.GetReg s
                        let r = match t with
                                | SmallIntByte -> (i <<< 24) >>> 24
                                | SmallIntChar -> i &&& 0xFFFF
                                | SmallIntShort -> (i <<< 16) >>> 16
                        this.SetReg (d, Store.storeInt r)
                        next ()

                    | Add (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | CoreInteger IntegerInt -> Store.storeInt <| Store.loadInt v1 + Store.loadInt v2
                            | CoreInteger IntegerLong -> Store.storeLong <| (Store.loadLong v1).Add (Store.loadLong v2)
                            | CoreFloating FloatingFloat -> Store.storeFloat <| Store.loadFloat v1 + Store.loadFloat v2
                            | CoreFloating FloatingDouble -> Store.storeDouble <| Store.loadDouble v1 + Store.loadDouble v2
                        this.SetReg(d, newval)
                        next ()
                    | Sub (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | CoreInteger IntegerInt -> Store.storeInt <| Store.loadInt v1 - Store.loadInt v2
                            | CoreInteger IntegerLong -> Store.storeLong <| (Store.loadLong v1).Subtract (Store.loadLong v2)
                            | CoreFloating FloatingFloat -> Store.storeFloat <| Store.loadFloat v1 - Store.loadFloat v2
                            | CoreFloating FloatingDouble -> Store.storeDouble <| Store.loadDouble v1 - Store.loadDouble v2
                        this.SetReg(d, newval)
                        next ()
                    | Mul (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | CoreInteger IntegerInt -> Store.storeInt <| Store.loadInt v1 * Store.loadInt v2
                            | CoreInteger IntegerLong -> Store.storeLong <| (Store.loadLong v1).Multiply (Store.loadLong v2)
                            | CoreFloating FloatingFloat -> Store.storeFloat <| Store.loadFloat v1 * Store.loadFloat v2
                            | CoreFloating FloatingDouble -> Store.storeDouble <| Store.loadDouble v1 * Store.loadDouble v2
                        this.SetReg(d, newval)
                        next ()
                    | Div (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | CoreInteger IntegerInt -> Store.storeInt <| Store.loadInt v1 / Store.loadInt v2
                            | CoreInteger IntegerLong -> Store.storeLong <| (Store.loadLong v1).Div (Store.loadLong v2)
                            | CoreFloating FloatingFloat -> Store.storeFloat <| Store.loadFloat v1 / Store.loadFloat v2
                            | CoreFloating FloatingDouble -> Store.storeDouble <| Store.loadDouble v1 / Store.loadDouble v2
                        this.SetReg(d, newval)
                        next ()
                    | Rem (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | CoreInteger IntegerInt -> Store.storeInt <| Store.loadInt v1 % Store.loadInt v2
                            | CoreInteger IntegerLong -> Store.storeLong <| (Store.loadLong v1).Modulo (Store.loadLong v2)
                            | CoreFloating FloatingFloat -> Store.storeFloat <| Store.loadFloat v1 % Store.loadFloat v2
                            | CoreFloating FloatingDouble -> Store.storeDouble <| Store.loadDouble v1 % Store.loadDouble v2
                        this.SetReg(d, newval)
                        next ()

                    | And (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | IntegerInt -> Store.storeInt <| (Store.loadInt v1 &&& Store.loadInt v2)
                            | IntegerLong -> Store.storeLong <| (Store.loadLong v1).And (Store.loadLong v2)
                        this.SetReg(d, newval)
                        next ()
                    | Or (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | IntegerInt -> Store.storeInt <| (Store.loadInt v1 ||| Store.loadInt v2)
                            | IntegerLong -> Store.storeLong <| (Store.loadLong v1).Or (Store.loadLong v2)
                        this.SetReg(d, newval)
                        next ()
                    | Xor (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | IntegerInt -> Store.storeInt <| (Store.loadInt v1 ^^^ Store.loadInt v2)
                            | IntegerLong -> Store.storeLong <| (Store.loadLong v1).Xor (Store.loadLong v2)
                        this.SetReg(d, newval)
                        next ()
                    | Shl (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | IntegerInt -> Store.storeInt <| (Store.loadInt v1 <<< (Store.loadInt v2 &&& 0x1F))
                            | IntegerLong -> Store.storeLong <| (Store.loadLong v1).ShiftLeft ((Store.loadLong v2).ToInt () &&& 0x1F)
                        this.SetReg(d, newval)
                        next ()
                    | Shr (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | IntegerInt -> Store.storeInt <| (Store.loadInt v1 >>> (Store.loadInt v2 &&& 0x1F))
                            | IntegerLong -> Store.storeLong <| (Store.loadLong v1).ShiftRight ((Store.loadLong v2).ToInt () &&& 0x1F)
                        this.SetReg(d, newval)
                        next ()
                    | Ushr (t, (d, a, b)) ->
                        let (v1, v2) = (this.GetReg a, this.GetReg b)
                        let newval =
                            match t with
                            | IntegerInt -> Store.storeInt << int32 <| ((uint32 <| Store.loadInt v1) >>> (Store.loadInt v2 &&& 0x1F))
                            | IntegerLong -> Store.storeLong <| (Store.loadLong v1).ShiftRightUnsigned ((Store.loadLong v2).ToInt () &&& 0x1F)
                        this.SetReg(d, newval)
                        next ()

                    | AddIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) + i)); next ()
                    | RsubIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (i - Store.loadInt (this.GetReg a))); next ()
                    | MulIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) * i)); next ()
                    | DivIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) / i)); next ()
                    | RemIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) % i)); next ()
                    | AndIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) &&& i)); next ()
                    | OrIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) ||| i)); next ()
                    | XorIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) ^^^ i)); next ()
                    | ShlIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) <<< (i &&& 0x1F))); next ()
                    | ShrIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt (Store.loadInt (this.GetReg a) >>> (i &&& 0x1F))); next ()
                    | UshrIntLit (d, a, i) ->
                        this.SetReg(d, Store.storeInt << int <| ((uint32 <| Store.loadInt (this.GetReg a)) >>> (i &&& 0x1F))); next ()
            let a = JavaScript.TypeOf JavaScript.Undefined
            ()

    [<JavaScript>]
    let mutable thread = None

    [<JavaScript>]
    let init ((dtype : Dex.Type, meth : Dex.Method, impl : Dex.MethodImpl),  args : RegValue array) =
        let t = new Thread ()
        thread <- Some t
        t.Start (dtype, meth, impl) args
