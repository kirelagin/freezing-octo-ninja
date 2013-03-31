namespace Dalvik

module ByteCode =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    // 4 bits
    type nibble = sbyte
    [<Inline "(new Int8Array([n << 4]))[0] >> 4">]
    let nibble (n : byte) : nibble = X<_>
    // Unsigned 4 bitw
    type unibble = byte
    [<JavaScript>]
    let unibble (n : byte) : unibble = n &&& 0x0Fuy

    type reg = float64
    [<JavaScript>]
    let reg n : reg = float64 n


    type Bias = | LtBias | GtBias
    type Test = | Eq | Ne | Lt | Ge | Gt | Le
    type InvokeKind = | InvokeVirtual | InvokeSuper | InvokeDirect | InvokeStatic | InvokeInterface
    type CodeOffset<'a> =
        | Unresolved of uint32 * 'a // (this instruction offset in 16bit units, relative offset in 16bit units)
        | Resolved of uint32        // (target instruction's index in the instructions array)

    [<JavaScript>]
    type Instruction =
        (* 00 *)    | Nop

        (* 01-03,
           07-09 *) | Move of reg * reg
        (* 04-06 *) | MoveWide of reg * reg

        (* 0a,0c *) | MoveResult of reg
        (* 0b *)    | MoveResultWide of reg

        (* 0d *)    | MoveException of reg

        (* 0e *)    | ReturnVoid
        (* 0f,11 *) | Return of reg
        (* 10 *)    | ReturnWide of reg

        (* 12 *)    | Const4 of reg * nibble
        (* 13 *)    | Const16 of reg * int16
        (* 14 *)    | Const of reg * int32
        (* 15 *)    | ConstHigh16 of reg * int16
        (* 16 *)    | ConstWide16 of reg * int16
        (* 17 *)    | ConstWide32 of reg * int32
        (* 18 *)    | ConstWide of reg * (int32 * int32)
        (* 19 *)    | ConstWideHigh16 of reg * int16
        (* 1a *)    | ConstString of reg * uint16
        (* 1b *)    | ConstStringJumdo of reg * uint32
        (* 1c *)    | ConstClass of reg * uint16

        (* 1d *)    | MonitorEnter of reg
        (* 1e *)    | MonitorExit of reg

        (* 1f *)    | CheckCast of reg * uint16
        (* 20 *)    | InstanceOf of reg * reg * uint16
        (* 21 *)    | ArrayLength of reg * reg
        (* 22 *)    | NewInstance of reg * uint16
        (* 23 *)    | NewArray of reg * reg * uint16
        (* 24 *)    (*| FilledNewArray of *)
        (* 25 *)    (*| FilledNewArrayRange of *)
        (* 26 *)    (*| FillArrayData of reg * CodeOffset<int32> *)

        (* 27 *)    | Throw of reg
        (* 28-2a *) | Goto of CodeOffset<int32>
        (* 2b *)    (*| PackedSwitch of *)
        (* 2c *)    (*| SparseSwitch of *)

        (* 2d-2e *) | CmpFloat of Bias * (reg * reg * reg)
        (* 2f-30 *) | CmpDouble of Bias * (reg * reg * reg)
        (* 31 *)    | CmpLong of reg * reg * reg

        (* 32-37 *) | If of Test * (reg * reg * CodeOffset<int16>)
        (* 38-3d *) | IfZ of Test * (reg * CodeOffset<int16>)

        (* 44,
           46-4a *) | Aget of reg * reg * reg
        (* 45 *)    | AgetWide of reg * reg * reg
        (* 4b,
           4d-51 *) | Aput of reg * reg * reg
        (* 4c *)    | AputWide of reg * reg * reg

        (* 52,
           54-58 *) | Iget of reg * reg * uint16
        (* 53 *)    | IgetWide of reg * reg * uint16
        (* 59,
           5b-5f *) | Iput of reg * reg * uint16
        (* 5a *)    | IputWide of reg * reg * uint16

        (* 60
           62-66 *) | Sget of reg * uint16
        (* 61 *)    | SgetWide of reg * uint16
        (* 67,
           69-6d *) | Sput of reg * uint16
        (* 68 *)    | SputWide of reg * uint16

        (* 6e-72 *) | Invoke of InvokeKind * (unibble * uint16 * reg * reg * reg * reg * reg)
        (* 74-78 *) (*| InvokeRange of InvokeKind * uint8 * uint16 * reg *)

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
                    | RsubInt of reg * reg * int32
                    | MulIntLit of reg * reg * int32
                    | DivIntLit of reg * reg * int32
                    | RemIntLit of reg * reg * int32
                    | AndIntLit of reg * reg * int32
                    | OrIntLit of reg * reg * int32
        (* ...e2 *) | XorIntLit of reg * reg * int32


    [<JavaScript>]
    module OpFormat =
        let read10x (stream : FileArray.DexFileArray) : unit =
            stream.GetByte () |> ignore

        let read12x (stream : FileArray.DexFileArray) : nibble * nibble =
            let ba = stream.GetByte ()
            (nibble <| ba, nibble <| ba >>> 4)
        let read11n (stream : FileArray.DexFileArray) : nibble * nibble =
            let ba = stream.GetByte ()
            (nibble <| ba, nibble <| ba >>> 4)

        let read11x (stream : FileArray.DexFileArray) : uint8 =
            stream.GetByte ()
        let read10t (stream : FileArray.DexFileArray) : int8 =
            stream.GetInt8 ()

        let read20t (stream : FileArray.DexFileArray) : int16 =
            stream.GetByte () |> ignore
            stream.GetInt16 ()

        let read22x (stream : FileArray.DexFileArray) : uint8 * uint16 =
            (stream.GetByte (), stream.GetUInt16 ())
        let read21t (stream : FileArray.DexFileArray) : uint8 * int16 =
            (stream.GetByte (), stream.GetInt16 ())
        let read21s (stream : FileArray.DexFileArray) : uint8 * int16 =
            (stream.GetByte (), stream.GetInt16 ())
        let read21h (stream : FileArray.DexFileArray) : uint8 * int16 =
            (stream.GetByte (), stream.GetInt16 ())
        let read21c (stream : FileArray.DexFileArray) : uint8 * uint16 =
            (stream.GetByte (), stream.GetUInt16 ())

        let read23x (stream : FileArray.DexFileArray) : uint8 * uint8 * byte =
            (stream.GetByte (), stream.GetByte (), stream.GetByte ())
        let read22b (stream : FileArray.DexFileArray) : uint8  * uint8 * int8 =
            (stream.GetByte (), stream.GetByte (), stream.GetInt8 ())

        let read22t (stream : FileArray.DexFileArray) : unibble  * unibble * int16 =
            let ba = stream.GetByte ()
            (unibble <| ba, unibble <| ba >>> 4, stream.GetInt16 ())
        let read22s (stream : FileArray.DexFileArray) : unibble  * unibble * int16 =
            let ba = stream.GetByte ()
            (unibble <| ba, unibble <| ba >>> 4, stream.GetInt16 ())
        let read22c (stream : FileArray.DexFileArray) : unibble  * unibble * uint16 =
            let ba = stream.GetByte ()
            (unibble <| ba, unibble <| ba >>> 4, stream.GetUInt16 ())

        let read30t (stream : FileArray.DexFileArray) : int32 =
            stream.GetInt32 ()

        let read32x (stream : FileArray.DexFileArray) : uint16 * uint16 =
            (stream.GetUInt16 (), stream.GetUInt16 ())

        let read31i (stream : FileArray.DexFileArray) : uint8 * int32 =
            (stream.GetByte (), stream.GetInt32 ())
        let read31t (stream : FileArray.DexFileArray) : uint8 * int32 =
            (stream.GetByte (), stream.GetInt32 ())
        let read31c (stream : FileArray.DexFileArray) : uint8 * uint32 =
            (stream.GetByte (), stream.GetUInt32 ())

        let read35c (stream : FileArray.DexFileArray) : unibble * uint16 * unibble * unibble * unibble * unibble * unibble =
            let ag = stream.GetByte ()
            let meth = stream.GetUInt16 ()
            let fe = stream.GetByte ()
            let dc = stream.GetByte ()
            (unibble <| ag >>> 4, meth, unibble dc, unibble <| dc >>> 4, unibble fe, unibble <| fe >>> 4, unibble ag)

        //let read3rc // TODO: opformat 3rc

        let read51l (stream : FileArray.DexFileArray) : uint8 * (int32 * int32) =
            (stream.GetByte (), stream.GetInt64 ())
