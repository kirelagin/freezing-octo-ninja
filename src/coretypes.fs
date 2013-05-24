namespace Dalvik

open IntelliFactory.WebSharper

[<JavaScript>]
module Coretypes =
    open IntelliFactory.WebSharper.Html5

    // Dalvik reference
    type dref = int

    // Dalvik register number
    type reg = float64
    [<JavaScript>]
    let reg n : reg = float64 n

    type RegValue =
        | Reg32 of int32
        | Reg64 of GLong
        | RegRef of dref


    type SmallIntegerType = | SmallIntByte
                            | SmallIntChar
                            | SmallIntShort

    type IntegerType = | IntegerInt
                       | IntegerLong

    type FloatingType = | FloatingFloat
                        | FloatingDouble

    type CoreNumberType = | CoreInteger of IntegerType
                          | CoreFloating of FloatingType


    module Store =
        let loadInt (v : RegValue) = match v with
                                        | Reg32 i -> i
                                        | _ -> failwith "Trying to load an int from non-32-bit value"

        let storeInt (i : int32) = Reg32 (int32 i)

        let loadLong (v : RegValue) = match v with
                                        | Reg64 i -> i
                                        | _ -> failwith "Trying to load a long from non-64-bit register"

        let storeLong (i : GLong) = Reg64 i

        let loadFloat (v : RegValue) = match v with
                                        | Reg32 i -> (new Float32Array((new Int32Array([| As<int16>(i) |])).Buffer)).Get(0uL) //TODO #4
                                        | _ -> failwith "Trying to load a float from non-32-bit-register"

        let storeFloat (i : float32) = Reg32 << As<int32> <| (new Int32Array((new Float32Array([| i |])).Buffer)).Get(0uL) //TODO #4

        let loadDouble (v : RegValue) = match v with
                                        | Reg64 gl -> (new Float64Array((new Int32Array([| As<int16>(gl.GetLowBits()); As<int16>(gl.GetHighBits()) |])).Buffer)).Get(0uL) //TODO #4
                                        | _ -> failwith "Trying to load a double from non-64-bit-register"

        let storeDouble (i : float64) = let r = new Int32Array((new Float64Array([| i |])).Buffer)
                                        Reg64 <| GLong.FromBits (As<int32>(r.Get(0uL)), As<int32>(r.Get(1uL))) //TODO #4


    //module NumberOps =
    //    type NumberOps<'t> = { opLoad : RegValue -> ^t; opStore : ^t -> RegValue
    //                           opAdd : ^t -> ^t -> ^t; opSub : ^t -> ^t -> ^t
    //                           opMul : ^t -> ^t -> ^t; opDiv : ^t -> ^t -> ^t; opRem : ^t -> ^t -> ^t }

    //    let numberOps (t : CoreNumberType) =
    //        match t with
    //        | CoreInteger IntegerInt ->
    //            { opLoad = Store.loadInt; opStore = Store.storeInt; opAdd = (+); opSub = (-); opMul = (*); opDiv = (/); opRem = (%) }
    //        | CoreInteger IntegerLong ->
    //            { opLoad = Store.loadLong; opStore = Store.storeLong; opAdd = (fun a -> fun b -> a.Add b); opSub = (fun a -> fun b -> a.Subtract b);
    //                                                                        opMul = (fun a -> fun b -> a.Multiply b); opDiv = (fun a -> fun b -> a.Div b); opRem = (fun a -> fun b -> a.Modulo b) }

    //    let floatOps = { opLoad = Store.loadFloat; opStore = Store.storeFloat; opAdd = (+); opSub = (-); opMul = (*); opDiv = (/); opRem = (%) }
    //    let doubleOps = { opLoad = Store.loadDouble; opStore = Store.storeDouble; opAdd = (+); opSub = (-); opMul = (*); opDiv = (/); opRem = (%) }

    [<JavaScript>]
    module Convert =
        let intToLong (i : int32) : GLong = GLong.FromInt i
        let intToFloat (i : int32) : float32 = (new Float32Array([| float32 i |])).Get(0uL)
        let intToDouble (i : int32) : float64 = float64 i

        let doubleToInt (i : float64) : int32 = if i > float64 System.Int32.MaxValue then System.Int32.MaxValue
                                                elif i < float64 System.Int32.MinValue then System.Int32.MinValue
                                                else int32 i
        let doubleToLong (i : float64) : GLong = if i = infinity then GLong.MAX_VALUE elif i = -infinity then GLong.MIN_VALUE else GLong.FromNumber i
        let doubleToFloat (i : float64) : float32 = (new Float32Array([| float32 i |])).Get(0uL) //TODO #11

        let longToInt (i : GLong) : int32 = i.ToInt()
        let longToDouble (i : GLong) : float64 = i.ToNumber()
        let longToFloat (i : GLong) : float32 = longToDouble i |> doubleToFloat

        let floatToDouble (i : float32) : float64 = float64 i
        let floatToInt (i : float32) : int32 = floatToDouble i |> doubleToInt
        let floatToLong(i : float32) : GLong = floatToDouble i |> doubleToLong
