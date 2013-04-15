namespace Dalvik

[<AutoOpen>]
module JsUtil =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<AutoOpen>]
    module Conversions =
        open IntelliFactory.WebSharper

        // This is any JS number, since all numbers in
        // JS are 64bit floating-point
        type float64 = double
        [<Inline "$0">]
        let inline float64 x : float64 = X<_>

        [<Inline "$0 & 65535">]
        let inline uint16 x : uint16 = uint16 x

        [<Inline "$0 | 0">]
        let inline int32 x : int32 = int32 x

        // Don't forget to call this when working with bit-operations
        // even if you think you already got an unsigned int!
        [<Inline "($0 | 0) >>> 0">]
        let inline uint32 x : uint32 = uint32 x

        [<Inline "$0">]
        let inline float32 x : float32 = float32 x

    module Array =
        [<Inline "$arr.push($x)">]
        let push (arr : 'a array) (x : 'a) : unit = X<_>

    module ObjectMap =
        type ObjectMap<'k, 'v> = class end

        [<Inline "{}">]
        let empty<'k, 'v> : ObjectMap<'k, 'v> = X<_>

        [<Inline "$m[$k] = $v">]
        let update<'k, 'v> (m : ObjectMap<'k, 'v>) (k : 'k) (v : 'v) : unit = X<_>

        [<Inline "$m[$k]">]
        let get<'k, 'v> (m : ObjectMap<'k, 'v>) (k : 'k) : 'v = X<_>

        [<JavaScript>]
        let lookup<'k, 'v> (m: ObjectMap<'k, 'v>) (k : 'k) : 'v option =
            if JavaScript.TypeOf (get m k) = JavaScript.Kind.Undefined then
                None
            else
                Some <| get m k

    module Number =
        [<Inline "isFinite($v)">]
        let isFinite (v : 'a) : bool = X<_>

    [<Name [| "gLong" |]>]
    [<Stub>]
    type GLong private () =
        static member MAX_VALUE : GLong = X<_>
        static member MIN_VALUE : GLong = X<_>

        [<Name "fromBits">]
        static member FromBits (lowBits : int32, highBits : int32) : GLong = X<_>
        [<Name "fromInt">]
        static member FromInt (value : int32) : GLong = X<_>
        [<Name "fromNumber">]
        static member FromNumber (value : float64) : GLong = X<_>

        [<Name "toInt">]
        member this.toInt () : int32 = X<_>
        [<Name "toNumber">]
        member this.toNumber () : float64 = X<_>
        [<Name "getHighBits">]
        member this.GetHighBits () : int32 = X<_>
        [<Name "getLowBits">]
        member this.GetLowBits () : int32 = X<_>
        
        [<Name "compare">]
        member this.Compare (other : GLong) : int32 = X<_>

        [<Name "negate">]
        member this.Negate () : GLong  = X<_>

        [<Name "add">]
        member this.Add (other : GLong) : GLong  = X<_>
        [<Name "subtract">]
        member this.Subtract (other : GLong) : GLong  = X<_>
        [<Name "multiply">]
        member this.Multiply (other : GLong) : GLong  = X<_>
        [<Name "div">]
        member this.Div (other : GLong) : GLong  = X<_>
        [<Name "modulo">]
        member this.Modulo (other : GLong) : GLong  = X<_>

        [<Name "and">]
        member this.And (other : GLong) : GLong  = X<_>
        [<Name "or">]
        member this.Or (other : GLong) : GLong  = X<_>
        [<Name "xor">]
        member this.Xor (other : GLong) : GLong  = X<_>
        [<Name "shiftLeft">]
        member this.ShiftLeft (numBit : int32) : GLong  = X<_>
        [<Name "shiftRight">]
        member this.ShiftRight (numBit : int32) : GLong  = X<_>
        [<Name "shiftRightUnsigned">]
        member this.ShiftRightUnsigned (numBit : int32) : GLong  = X<_>

    [<JavaScript>]
    type JsValue =
        | Js32 of int32
        | Js64 of GLong
        | JsRef of obj

    [<JavaScript>]
    module Store =
        let loadInt (v : JsValue) = match v with
                                        | Js32 i -> i
                                        | _ -> failwith "Trying to load an int from non-32-bit value"

        let storeInt (i : int32) = Js32 i

        let loadLong (v : JsValue) = match v with
                                        | Js64 i -> i
                                        | _ -> failwith "Trying to load a long from non-64-bit register"

        let storeLong (i : GLong) = Js64 i

        let loadFloat (v : JsValue) = match v with
                                        | Js32 i -> (new Float32Array((new Int32Array([| As<int16>(i) |])).Buffer)).Get(0uL) //TODO #4
                                        | _ -> failwith "Trying to load a float from non-32-bit-register"

        let storeFloat (i : float32) = Js32 << As<int32> <| (new Int32Array((new Float32Array([| i |])).Buffer)).Get(0uL) //TODO #4

        let loadDouble (v : JsValue) = match v with
                                        | Js64 gl -> (new Float64Array((new Int32Array([| As<int16>(gl.GetHighBits()); As<int16>(gl.GetLowBits()) |])).Buffer)).Get(0uL) //TODO #4
                                        | _ -> failwith "Trying to load a double from non-64-bit-register"

        let storeDouble (i : float64) = let r = new Int32Array((new Float64Array([| i |])).Buffer)
                                        Js64 <| GLong.FromBits (As<int32>(r.Get(1uL)), As<int32>(r.Get(0uL))) //TODO #4

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
