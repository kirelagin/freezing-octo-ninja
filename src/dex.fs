namespace Dalvik

module Dex =
    open IntelliFactory.WebSharper

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

    type Bias = | LtBias | GtBias
    type Test = | Eq | Ne | Lt | Ge | Gt | Le
    type InvokeKind = | InvokeVirtual | InvokeSuper | InvokeDirect | InvokeStatic | InvokeInterface
    type CodeOffset =
        | RelativeBytes of int32    // (relative offset in 16bit units)
        | AbsoluteIndex of int32    // (target instruction's index in the instructions array)

    type
     [<JavaScript>]
     Type =
        //val mutable desriptor : string
        [<DefaultValue>] val mutable desriptor : string
        [<DefaultValue>] val mutable cls : Class
        //new (descriptor0) = { desriptor = descriptor0 }
        new () = {} //TODO #14
        static member Mew (descriptor0) =
            let t = Type()
            t.desriptor <- descriptor0
            t
    and
     [<JavaScript>]
     Proto =
        //val mutable shorty : string
        //val mutable return_type : Type
        //val mutable parameters : Type array
        [<DefaultValue>] val mutable shorty : string
        [<DefaultValue>] val mutable return_type : Type
        [<DefaultValue>] val mutable parameters : Type array
        //new (shorty0, return_type0, parameters0) = { shorty = shorty0; return_type = return_type0; parameters = parameters0 }
        new () = {} //TODO #14
        static member Mew (shorty0, return_type0, parameters0) =
            let p = Proto()
            p.shorty <- shorty0
            p.return_type <- return_type0
            p.parameters <- parameters0
            p
    and
     [<JavaScript>]
     Field =
        //val mutable dtype : Type
        //val mutable name : string
        [<DefaultValue>] val mutable dtype : Type
        [<DefaultValue>] val mutable name : string
        [<DefaultValue>] val mutable access_flags : uint32
        //new (dclass0, dtype0, name0) = { dtype = dtype0; name = name0 }
        new () = {} //TODO #14
        static member Mew (dclass0, dtype0, name0) =
            let f = Field()
            f.dtype <- dtype0
            f.name <- name0
            f
    and
     [<JavaScript>]
     Method =
        //val mutable proto : Proto
        //val mutable name : string
        [<DefaultValue>] val mutable proto : Proto
        [<DefaultValue>] val mutable name : string
        [<DefaultValue>] val mutable access_flags : uint32
        [<DefaultValue>] val mutable registers_size : uint16
        [<DefaultValue>] val mutable ins_size : uint16
        [<DefaultValue>] val mutable outs_size : uint16
        [<DefaultValue>] val mutable insns : Instruction array
        //new (dclass0, proto0, name0) = { proto = proto0; name = name0 }
        new () = {} //TODO #14
        static member Mew (dclass0, proto0, name0) =
            let m = Method()
            m.proto <- proto0
            m.name <- name0
            m
    and
     [<JavaScript>]
     Class  =
        [<DefaultValue>] val mutable dclass : Type
        //TODO #6 (access_flags)
        [<DefaultValue>] val mutable super : Type option
        [<DefaultValue>] val mutable interfaces : Type array
        [<DefaultValue>] val mutable direct_methods : Method array
        [<DefaultValue>] val mutable virtual_methods : Method array
        //TODO #5 (annotations)
        //TODO #2 (static_values)
        //new (...)
        new () = {} //TODO #14
        static member New (dclass0, access_flags0, superclass0, interfaces0, source_file0, static_fields0, instance_fields0, direct_methods0, virtual_methods0, static_values0) =
            let c = Class()
            c.dclass <- dclass0
            c.super <- superclass0
            c.interfaces <- interfaces0
            c.direct_methods <- direct_methods0
            c.virtual_methods <- virtual_methods0
            dclass0.cls <- c
            c
    and
     [<JavaScript>]
     Instruction =
        (* 00 *)    | Nop of unit

        (* 01-03,
           07-09 *) | Move of reg * reg
        (* 04-06 *) | MoveWide of reg * reg

        (* 0a,0c *) | MoveResult of reg
        (* 0b *)    | MoveResultWide of reg

        (* 0d *)    | MoveException of reg

        (* 0e *)    | ReturnVoid of unit
        (* 0f,11 *) | Return of reg
        (* 10 *)    | ReturnWide of reg

        (* 12 *)    | Const4 of reg * nibble
        (* 13 *)    | Const16 of reg * int16
        (* 14 *)    | Const of reg * int32
        (* 15 *)    | ConstHigh16 of reg * int16
        (* 16 *)    | ConstWide16 of reg * int16
        (* 17 *)    | ConstWide32 of reg * int32
        (* 18 *)    | ConstWide of reg * GLong
        (* 19 *)    | ConstWideHigh16 of reg * int16
        (* 1a *)    | ConstString of reg * string
        (* 1b *)    | ConstStringJumdo of reg * string
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
        (* 26 *)    (*| FillArrayData of reg * CodeOffset<int32> *) //TODO #9

        (* 27 *)    | Throw of reg
        (* 28-2a *) | Goto of CodeOffset
        (* 2b *)    (*| PackedSwitch of *) //TODO #9
        (* 2c *)    (*| SparseSwitch of *) //TODO #9

        (* 2d-2e *) | CmpFloat of Bias * (reg * reg * reg)
        (* 2f-30 *) | CmpDouble of Bias * (reg * reg * reg)
        (* 31 *)    | CmpLong of reg * reg * reg

        (* 32-37 *) | If of Test * (reg * reg * CodeOffset)
        (* 38-3d *) | IfZ of Test * (reg * CodeOffset)

        (* 44,
           46-4a *) | Aget of reg * reg * reg
        (* 45 *)    | AgetWide of reg * reg * reg
        (* 4b,
           4d-51 *) | Aput of reg * reg * reg
        (* 4c *)    | AputWide of reg * reg * reg

        (* 52,
           54-58 *) | Iget of reg * reg * Field
        (* 53 *)    | IgetWide of reg * reg * Field
        (* 59,
           5b-5f *) | Iput of reg * reg * Field
        (* 5a *)    | IputWide of reg * reg * Field

        (* 60
           62-66 *) | Sget of reg * Field
        (* 61 *)    | SgetWide of reg * Field
        (* 67,
           69-6d *) | Sput of reg * Field
        (* 68 *)    | SputWide of reg * Field

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
        (* ...e2 *) | XorIntLit of reg * reg * int32
