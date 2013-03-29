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
        type float64 = int64
        [<Inline "$0">]
        let inline float64 x = X<_>

        [<Inline "$0 & 65535">]
        let inline uint16 x = uint16 x

        [<Inline "$0 | 0">]
        let inline int32 x = int32 x

        // Don't forget to call this when working with bit-operations
        // even if you think you already got an unsigned int!
        [<Inline "($0 | 0) >>> 0">]
        let inline uint32 x = uint32 x

    module Array =
        [<Inline "$arr.push($x)">]
        let push (arr : 'a array) (x : 'a) : unit = X<_>

    [<Name [| "gLong" |]>]
    [<Stub>]
    type GLong private () =
        [<Name "fromBits">]
        static member FromBits (lowBits : uint32, highBits : uint32) : GLong = X<_>

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
        | JsNumber of float64
        | JsLong of GLong
        | JsRef of obj