namespace Dalvik

module Dex =
    open Dumbdict
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5
    open Coretypes

    // 4 bits
    type nibble = sbyte
    [<Inline "(new Int8Array([$n << 4]))[0] >> 4">]
    let nibble (n : byte) : nibble = X<_>
    // Unsigned 4 bitw
    type unibble = byte
    [<JavaScript>]
    let unibble (n : byte) : unibble = n &&& 0x0Fuy


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

    type StaticValue = | StaticReg of RegValue
                       | StaticString of string
                       | StaticArray of StaticValue array

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
     Class = Class of Type * uint32 * Type option * Type array * ClassImpl option * StaticValue array
     //TODO #5 (annotations)
     //TODO #2 (static_values)
    and
     [<JavaScript>]
     ClassImpl = ClassImpl of (Field * uint32) array * dumbdict<Field, uint32> // order matter for static init
                              (* static *)             (* instance *)
                            * dumbdict<Method, uint32 * MethodImpl option> * dumbdict<Method, uint32 * MethodImpl option>
                              (* direct *)                                   (* virtual *)
    and
     [<JavaScript>]
     Instruction =
        (* 00 *)    | Nop of unit                       // 0

        (* 01-09 *) | Move of reg * reg
        (* 0a-0c *) | MoveResult of reg
        (* 0d *)    | MoveException of reg

        (* 0e *)    | ReturnVoid of unit
        (* 0f-11 *) | Return of reg

        (* 12 *)    | Const4 of reg * nibble
        (* 13 *)    | Const16 of reg * int16
        (* 14 *)    | Const of reg * int32
        (* 15 *)    | ConstHigh16 of reg * int16
        (* 16 *)    | ConstWide16 of reg * int16        // 10
        (* 17 *)    | ConstWide32 of reg * int32
        (* 18 *)    | ConstWide of reg * GLong
        (* 19 *)    | ConstWideHigh16 of reg * int16
        (* 1a *)    | ConstString of reg * string
        (* 1c *)    | ConstClass of reg * Type

        (* 1d *)    | MonitorEnter of reg
        (* 1e *)    | MonitorExit of reg

        (* 1f *)    | CheckCast of reg * Type
        (* 20 *)    | InstanceOf of reg * reg * Type
        (* 21 *)    | ArrayLength of reg * reg          // 20
        (* 22 *)    | NewInstance of reg * Type
        (* 23 *)    | NewArray of reg * reg * Type
        (* 24 *)    | FilledNewArray of (unibble * Type * reg * reg * reg * reg * reg )
        (* 25 *)    | FilledNewArrayRange of (uint8 * Type * reg)
        (* 26 *)    | FillArrayData of reg * RegValue array

        (* 27 *)    | Throw of reg
        (* 28-2a *) | Goto of CodeOffset
        (* 2b-2c *) | Switch of reg * (int * CodeOffset) array

        (* 2d-2e *) | CmpFloat of Bias * (reg * reg * reg)
        (* 2f-30 *) | CmpDouble of Bias * (reg * reg * reg)     // 30
        (* 31 *)    | CmpLong of reg * reg * reg

        (* 32-37 *) | If of Test * (reg * reg * CodeOffset)
        (* 38-3d *) | IfZ of Test * (reg * CodeOffset)

        (* 44-4a *) | Aget of reg * reg * reg
        (* 4b-51 *) | Aput of reg * reg * reg

        (* 52-58 *) | Iget of reg * reg * Field
        (* 59-5f *) | Iput of reg * reg * Field

        (* 60-66 *) | Sget of reg * Field
        (* 67-6d *) | Sput of reg * Field

        (* 6e-72 *) | Invoke of InvokeKind * (unibble * Method * reg * reg * reg * reg * reg)   // 40
        (* 74-78 *) | InvokeRange of InvokeKind * (uint8 * Method * reg)

        (* 7b... *) | Neg of CoreNumberType * (reg * reg)
                    | Not of IntegerType * (reg * reg)

                    | Convert of (CoreNumberType * CoreNumberType) * (reg * reg)
        (* ...8f *) | IntToSmall of SmallIntegerType * (reg * reg)

        (* 90... *) | Add of CoreNumberType * (reg * reg * reg)
                    | Sub of CoreNumberType * (reg * reg * reg)
                    | Mul of CoreNumberType * (reg * reg * reg)
                    | Div of CoreNumberType * (reg * reg * reg)
                    | Rem of CoreNumberType * (reg * reg * reg)     // 50

                    | And  of IntegerType * (reg * reg * reg)
                    | Or   of IntegerType * (reg * reg * reg)
                    | Xor  of IntegerType * (reg * reg * reg)
                    | Shl  of IntegerType * (reg * reg * reg)
                    | Shr  of IntegerType * (reg * reg * reg)
        (* ...cf *) | Ushr of IntegerType * (reg * reg * reg)

        (* d0... *) | AddIntLit  of reg * reg * int32
                    | RsubIntLit of reg * reg * int32
                    | MulIntLit  of reg * reg * int32
                    | DivIntLit  of reg * reg * int32
                    | RemIntLit  of reg * reg * int32
                    | AndIntLit  of reg * reg * int32
                    | OrIntLit   of reg * reg * int32
                    | XorIntLit  of reg * reg * int32
                    | ShlIntLit  of reg * reg * int32
                    | ShrIntLit  of reg * reg * int32
        (* ...e2 *) | UshrIntLit of reg * reg * int32



    type PrimitiveType = | BooleanType
                         | ByteType
                         | ShortType
                         | CharType
                         | IntType
                         | LongType
                         | FloatType
                         | DoubleType

    and  ReferenceType = | ObjectType of Type
                         | ArrayType of JavaType

    and  JavaType = | JavaReferenceType of ReferenceType
                    | JavaPrimitiveType of PrimitiveType