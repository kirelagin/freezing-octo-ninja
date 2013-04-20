namespace Dalvik

module Dex =
    open Dumbdict
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    // 4 bits
    type nibble = sbyte
    [<Inline "(new Int8Array([$n << 4]))[0] >> 4">]
    let nibble (n : byte) : nibble = X<_>
    // Unsigned 4 bitw
    type unibble = byte
    [<JavaScript>]
    let unibble (n : byte) : unibble = n &&& 0x0Fuy

    type reg = float64
    [<JavaScript>]
    let reg n : reg = float64 n

    // Dalvi reference
    type dref = int

    [<JavaScript>]
    type RegValue =
        | Reg32 of int32
        | Reg64 of GLong
        | RegRef of dref
        | RegAny of obj

    [<JavaScript>]
    module Store =
        let loadInt (v : RegValue) = match v with
                                        | Reg32 i -> i
                                        | _ -> failwith "Trying to load an int from non-32-bit value"

        let storeInt (i : int32) = Reg32 i

        let loadLong (v : RegValue) = match v with
                                        | Reg64 i -> i
                                        | _ -> failwith "Trying to load a long from non-64-bit register"

        let storeLong (i : GLong) = Reg64 i

        let loadFloat (v : RegValue) = match v with
                                        | Reg32 i -> (new Float32Array((new Int32Array([| As<int16>(i) |])).Buffer)).Get(0uL) //TODO #4
                                        | _ -> failwith "Trying to load a float from non-32-bit-register"

        let storeFloat (i : float32) = Reg32 << As<int32> <| (new Int32Array((new Float32Array([| i |])).Buffer)).Get(0uL) //TODO #4

        let loadDouble (v : RegValue) = match v with
                                        | Reg64 gl -> (new Float64Array((new Int32Array([| As<int16>(gl.GetHighBits()); As<int16>(gl.GetLowBits()) |])).Buffer)).Get(0uL) //TODO #4
                                        | _ -> failwith "Trying to load a double from non-64-bit-register"

        let storeDouble (i : float64) = let r = new Int32Array((new Float64Array([| i |])).Buffer)
                                        Reg64 <| GLong.FromBits (As<int32>(r.Get(1uL)), As<int32>(r.Get(0uL))) //TODO #4


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

        let longToInt (i : GLong) : int32 = i.toInt()
        let longToDouble (i : GLong) : float64 = i.toNumber()
        let longToFloat (i : GLong) : float32 = longToDouble i |> doubleToFloat

        let floatToDouble (i : float32) : float64 = float64 i
        let floatToInt (i : float32) : int32 = floatToDouble i |> doubleToInt
        let floatToLong(i : float32) : GLong = floatToDouble i |> doubleToLong

    type Bias = | LtBias | GtBias
    type Test = | Eq | Ne | Lt | Ge | Gt | Le
    type InvokeKind = | InvokeVirtual | InvokeSuper | InvokeDirect | InvokeStatic | InvokeInterface
    type CodeOffset =
        | RelativeBytes of int32    // (relative offset in 16bit units)
        | AbsoluteIndex of int32    // (target instruction's index in the instructions array)

    type
     [<JavaScript>]
     Type = Type of string
    and
     [<JavaScript>]
     Proto = Proto of string * Type * Type array
    and
     [<JavaScript>]
     Field = Field of Type * Type * string
    and
     [<JavaScript>]
     Method = Method of Type * Proto * string
    and
     [<JavaScript>]
     MethodImpl = MethodImpl of (* registers_size *) uint16
                              * (* ins_size *) uint16
                              * (* outs_size *) uint16
                              * (* insns *) Instruction array
    and
     [<JavaScript>]
     Class = Class of Type * uint32 * Type option * Type array * ClassImpl option
     //TODO #5 (annotations)
     //TODO #2 (static_values)
    and
     [<JavaScript>]
     ClassImpl = ClassImpl of dumbdict<Field, uint32> * dumbdict<Field, uint32>
                              (* static *)                (* instane *)
                            * dumbdict<Method, uint32 * MethodImpl option> * dumbdict<Method, uint32 * MethodImpl option>
                              (* direct *)                                     (* virtual *)
    and
     [<JavaScript>]
     Instruction =
        (* 00 *)    | Nop of unit

        (* 01-09 *) | Move of reg * reg
        (* 0a-0c *) | MoveResult of reg
        (* 0d *)    | MoveException of reg

        (* 0e *)    | ReturnVoid of unit
        (* 0f-11 *) | Return of reg

        (* 12 *)    | Const4 of reg * nibble
        (* 13 *)    | Const16 of reg * int16
        (* 14 *)    | Const of reg * int32
        (* 15 *)    | ConstHigh16 of reg * int16
        (* 16 *)    | ConstWide16 of reg * int16
        (* 17 *)    | ConstWide32 of reg * int32
        (* 18 *)    | ConstWide of reg * GLong
        (* 19 *)    | ConstWideHigh16 of reg * int16
        (* 1a *)    | ConstString of reg * string
        (* 1c *)    | ConstClass of reg * Type

        (* 1d *)    | MonitorEnter of reg
        (* 1e *)    | MonitorExit of reg

        (* 1f *)    | CheckCast of reg * Type
        (* 20 *)    | InstanceOf of reg * reg * Type
        (* 21 *)    | ArrayLength of reg * reg
        (* 22 *)    | NewInstance of reg * Type
        (* 23 *)    | NewArray of reg * reg * Type
        (* 24 *)    | FilledNewArray of (unibble * Type * reg * reg * reg * reg * reg )
        (* 25 *)    | FilledNewArrayRange of (uint8 * Type * reg)
        (* 26 *)    | FillArrayData of reg * RegValue array

        (* 27 *)    | Throw of reg
        (* 28-2a *) | Goto of CodeOffset
        (* 2b-2c *) | Switch of reg * (int * CodeOffset) array

        (* 2d-2e *) | CmpFloat of Bias * (reg * reg * reg)
        (* 2f-30 *) | CmpDouble of Bias * (reg * reg * reg)
        (* 31 *)    | CmpLong of reg * reg * reg

        (* 32-37 *) | If of Test * (reg * reg * CodeOffset)
        (* 38-3d *) | IfZ of Test * (reg * CodeOffset)

        (* 44-4a *) | Aget of reg * reg * reg
        (* 4b-51 *) | Aput of reg * reg * reg

        (* 52-58 *) | Iget of reg * reg * Field
        (* 59-5f *) | Iput of reg * reg * Field

        (* 60-66 *) | Sget of reg * Field
        (* 67-6d *) | Sput of reg * Field

        (* 6e-72 *) | Invoke of InvokeKind * (unibble * Method * reg * reg * reg * reg * reg)
        (* 74-78 *) | InvokeRange of InvokeKind * (uint8 * Method * reg)

        (* 7b... *) | NegInt of reg * reg
                    | NotInt of reg * reg
                    | NegLong of reg * reg
                    | NotLong of reg * reg
                    | NegFloat of reg * reg
                    | NegDouble of reg * reg
                    | IntToLong of reg * reg
                    | IntToFloat of reg * reg
                    | IntToDouble of reg * reg
                    | LongToInt of reg * reg
                    | LongToFloat of reg * reg
                    | LongToDouble of reg * reg
                    | FloatToInt of reg * reg
                    | FloatToLong of reg * reg
                    | FloatToDouble of reg * reg
                    | DoubleToInt of reg * reg
                    | DoubleToLong of reg * reg
                    | DoubleToFloat of reg * reg
                    | IntToByte of reg * reg
                    | IntToChar of reg * reg
        (* ...8f *) | IntToShort of reg * reg

        (* 90... *) | AddInt of reg * reg * reg
                    | SubInt of reg * reg * reg
                    | MulInt of reg * reg * reg
                    | DivInt of reg * reg * reg
                    | RemInt of reg * reg * reg
                    | AddLong of reg * reg * reg
                    | SubLong of reg * reg *reg
                    | MulLong of reg * reg * reg
                    | DivLong of reg * reg * reg
                    | RemLong of reg * reg * reg
                    | AddFloat of reg * reg * reg
                    | SubFloat of reg * reg * reg
                    | MulFloat of reg * reg * reg
                    | DivFloat of reg * reg * reg
                    | RemFloat of reg * reg * reg
                    | AddDouble of reg * reg * reg
                    | SubDouble of reg * reg * reg
                    | MulDouble of reg * reg * reg
                    | DivDouble of reg * reg * reg
                    | RemDouble of reg * reg * reg
                    | AndInt of reg * reg * reg
                    | OrInt of reg * reg * reg
                    | XorInt of reg * reg * reg
                    | ShlInt of reg * reg * reg
                    | ShrInt of reg * reg * reg
                    | UshrInt of reg * reg * reg
                    | AndLong of reg * reg * reg
                    | OrLong of reg * reg * reg
                    | XorLong of reg * reg * reg
                    | ShlLong of reg * reg * reg
                    | ShrLong of reg * reg * reg
        (* ...cf *) | UshrLong of reg * reg * reg

        (* d0... *) | AddIntLit of reg * reg * int32
                    | RsubIntLit of reg * reg * int32
                    | MulIntLit of reg * reg * int32
                    | DivIntLit of reg * reg * int32
                    | RemIntLit of reg * reg * int32
                    | AndIntLit of reg * reg * int32
                    | OrIntLit of reg * reg * int32
                    | XorIntLit of reg * reg * int32
                    | ShlIntLit of reg * reg * int32
                    | ShrIntLit of reg * reg * int32
        (* ...e2 *) | UshrIntLit of reg * reg * int32
