namespace Dalvik

module ByteCode =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    // 4 bits
    type nibble = int

    type reg = uint32


    type Bias = | LtBias | GtBias
    type Test = | Eq | Ne | Lt | Ge | Gt | Le
    type InvokeKind = | InvokeVirtual | InvokeSuper | InvokeDirect | InvokeStatic | InvokeInterface

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
        (* 18 *)    | ConstWide of reg * int64
        (* 19 *)    | ConstWideHigh16 of reg * int16
        (* 1a *)    | ConstString of reg * uint16
        (* 1b *)    | ConstStringJumdo of reg * int32
        (* 1c *)    | ConstClass of reg * int16

        (* 1d *)    | MonitorEnter of reg
        (* 1e *)    | MonitorExit of reg

        (* 1f *)    | CheckCast of reg * int16
        (* 20 *)    | InstanceOf of reg * reg * int16

        (* 21 *)    | ArrayLength of reg * reg

        (* 22 *)    | NewInstance of reg * int16
        (* 23 *)    | NewArray of reg * reg * int16
        (* 24 *)    (*| FilledNewArray of *)
        (* 25 *)    (*| FilledNewArrayRange of *)
        (* 26 *)    | FillArrayData of reg * int32

        (* 27 *)    | Throw of reg

        (* 28-2a *) | Goto of int32

        (* 2b *)    (*| PackedSwitch of *)
        (* 2c *)    (*| SparseSwitch of *)

        (* 2d-2e *) | CmpFloat of Bias * reg * reg * reg
        (* 2f-30 *) | CmpDouble of Bias * reg * reg * reg
        (* 31 *)    | CmpLong of reg * reg * reg

        (* 32-37 *) | If of Test * reg * reg * int16
        (* 38-3d *) | IfZ of Test * reg * int16

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

        (* 6e-72 *) | Invoke of InvokeKind * nibble * uint16 * reg * reg * reg * reg * reg
        (* 74-78 *) (*| InvokeRange of InvokeKind * uint8 * uint16 * reg *)

        (* 7b... *) | NegInt of reg * reg
                    | NotInt of reg * reg
                    | NegLong of reg * reg
                    | NotLong of reg * reg
                    | NegFloat of reg * reg
                    | NotFloat of reg * reg
                    | NegDouble of reg * reg
                    | NotDouble of reg * reg
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

        (* 90... *) | Add of reg * reg * reg (* for int, float and double... *)
                    | Sub of reg * reg * reg
                    | Mul of reg * reg * reg
                    | Div of reg * reg * reg
                    | Rem of reg * reg * reg (* ... done. *)
                    | AddLong of reg * reg * reg
                    | SubLong of reg * reg *reg
                    | MulLong of reg * reg * reg
                    | DivLong of reg * reg * reg
                    | RemLong of reg * reg * reg
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

        (* d0... *) | AddIntLit of reg * reg * int
                    | RsubInt of reg * reg * int
                    | MulIntLit of reg * reg * int
                    | DivIntLit of reg * reg * int
                    | RemIntLit of reg * reg * int
                    | AndIntLit of reg * reg * int
                    | OrIntLit of reg * reg * int
        (* ...e2 *) | XorIntLit of reg * reg * int


    [<JavaScript>]
    let Read (stream : FileArray.DexFileArray) =
        // TODO
        [| |]