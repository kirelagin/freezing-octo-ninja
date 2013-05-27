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
        
        [<Inline "$arr.pop()">]
        let pop (arr : 'a array) : 'a = X<_>

        [<Inline "$arr.shift()">]
        let shift (arr : 'a array) : 'a = X<_>

    module Number =
        [<Inline "isFinite($v)">]
        let isFinite (v : 'a) : bool = X<_>

    module String =
        [<Inline "unescape(encodeURIComponent($s)).split('').map(function(c){return c.charCodeAt()})">]
        let toByteArray (s : string) : byte array = X<_>

        [<Inline "$s.split('').map(function(c){return c.charCodeAt()})">]
        let toIntArray (s : string) : int array = X<_>

        [<Inline "String.fromCharCode.apply(null, $bs)">]
        let fromIntArray (bs : int array) : string = X<_>

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
        member this.ToInt () : int32 = X<_>
        [<Name "toNumber">]
        member this.ToNumber () : float64 = X<_>
        [<Name "toString">]
        override this.ToString () : string = X<_>
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

        [<Name "not">]
        member this.Not () : GLong  = X<_>
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


    [<Name [| "Date" |]>]
    [<Stub>]
    type Date () =
        [<Name "getTime">]
        member this.GetTime () : float64 = X<_>

    type ThreadId private () =
        [<Inline "$a === $b">]
        static member JsCompare (a : ThreadId, b : ThreadId) : bool = X<_>
