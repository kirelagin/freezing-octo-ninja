namespace Dalvik

module FileArray =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<Name [| "DataView" |]>]
    [<Stub>]
    type DataView (data : ArrayBuffer) =
        [<Name "getInt8">]
        member this.GetInt8(byteOffset : float64) : int8 = X<_>
        [<Name "getUint8">]
        member this.GetUint8(byteOffset : float64) : uint8 = X<_>
        [<Name "getInt16">]
        member this.GetInt16(byteOffset : float64, littleEndian : bool) : int16 = X<_>
        [<Name "getUint16">]
        member this.GetUint16(byteOffset : float64, littleEndian : bool) : uint16 = X<_>
        [<Name "getInt32">]
        member this.GetInt32(byteOffset : float64, littleEndian : bool) : int32 = X<_>
        [<Name "getUint32">]
        member this.GetUint32(byteOffset : float64, littleEndian : bool) : uint32 = X<_>
        [<Name "getFloat32">]
        member this.GetFloat32(byteOffset : float64, littleEndian : bool) : float32 = X<_>
        [<Name "getFloat64">]
        member this.GetFloat64(byteOffset : float64, littleEndian : bool) : double = X<_>

    [<JavaScript>]
    type FileArray (data : ArrayBuffer) =
        let mutable offset = 0u
        let view = new DataView(data)

        let getByte () : byte =
            let r = view.GetUint8 (float64 offset)
            offset <- offset + 1u
            r
        member this.GetBytes (count : int) : byte array =
            Array.init count (fun _ -> getByte())

        member this.Length = data.Length
        member this.Seek (newOffset : uint32) =
            let old = offset
            offset <- newOffset
            old
        member this.Offset = offset

        member this.GetBytesVar (given : int) (expected : int) (signExtend : bool) : byte array =
            let d = this.GetBytes given
            let fill = if signExtend && (d.[given-1] &&& 0x80uy) <> 0uy then 0xFFuy else 0x00uy
            Array.append d (Array.create (expected - given) fill)

        member this.GetByte () : byte =
            getByte ()
        member this.GetInt8 () : int8 =
            let r = view.GetInt8 (float64 offset)
            offset <- offset + 1u
            r
        member this.GetInt16 () : int16 =
            let r = view.GetInt16 (float64 offset, true)
            offset <- offset + 2u
            r
        member this.GetInt16Var (given : int) : int16 =
            let n = new Int16Array((new Uint8Array(this.GetBytesVar given 2 true)).Buffer)
            n.Get(0uL)
        member this.GetUInt16 () : uint16 =
            let r = view.GetUint16 (float64 offset, true)
            offset <- offset + 2u
            r
        member this.GetUInt16Var (given : int) : uint16 =
            let n = new Uint16Array((new Uint8Array(this.GetBytesVar given 2 false)).Buffer)
            n.Get(0uL)
        member this.GetInt32 () : int32 =
            let r = view.GetInt32 (float64 offset, true)
            offset <- offset + 4u
            r
        member this.GetInt32Var (given : int) : int32 =
            let n = new Int32Array((new Uint8Array(this.GetBytesVar given 4 true)).Buffer)
            int32 <| n.Get(0uL) //TODO #4
        member this.GetUInt32 () : uint32 =
            let r = view.GetUint32 (float64 offset, true)
            offset <- offset + 4u
            r
        member this.GetUInt32Var (given : int) : uint32 =
            let n = new Uint32Array((new Uint8Array(this.GetBytesVar given 4 false)).Buffer)
            uint32 <| n.Get(0uL) //TODO #4
        member this.GetInt64 () : int32 * int32 =
            (this.GetInt32 (), this.GetInt32 ())
        member this.GetInt64Var (given : int) : int32 * int32 =
            let n = new Int32Array((new Uint8Array(this.GetBytesVar given 8 true)).Buffer)
            (As<int32>(n.Get 0uL), As<int32>(n.Get 1uL)) //TODO #4

        member this.GetFloatVar (given : int) : float32 = //TODO #3
            let n = new Float32Array((new Uint8Array(this.GetBytesVar given 4 false)).Buffer)
            n.Get(0uL)
        member this.GetDoubleVar (given : int) : double = //TODO #3
            let n = new Float64Array((new Uint8Array(this.GetBytesVar given 8 false)).Buffer)
            n.Get(0uL)

    [<JavaScript>]
    type DexFileArray (data : ArrayBuffer) =
        inherit FileArray(data)

        member private this.GetLeb128 (signed : bool) : int32 =
            // FIXME: Awful imperative implementation (stolen from `dx`)
            let mutable result = 0
            let mutable count = 0
            let mutable signBits = -1
            let mutable stop = false
            while not stop do
                let cur = int <| this.GetByte ()
                result <- result ||| ((cur &&& 0x7F) <<< (count * 7))
                signBits <- signBits <<< 7
                count <- count + 1
                if (cur &&& 0x80) = 0 then
                    stop <- true 
            if signed && ((signBits >>> 1) &&& result) <> 0 then
                result ||| signBits
            else
                result
        member this.GetULeb128 () : uint32 = uint32 (this.GetLeb128 false)
        member this.GetSLeb128 () : int32 = this.GetLeb128 true

        member this.GetMUTF8String () : string =
            let out : int array = [| |]
            let mutable stop = false
            while not stop do
                let a = int <| this.GetByte ()
                if a = 0 then
                    stop <- true
                else
                    if a < 0x80 then
                        Array.push out a
                    else if (a &&& 0xE0) = 0xC0 then
                        let b = int <| this.GetByte ()
                        if ((b &&& 0xC0) <> 0x80) then failwith "MUTF-8: Bad second byte"
                        Array.push out (((a &&& 0x1F) <<< 6) ||| (b &&& 0x3F))
                    else if (a &&& 0xF0) = 0xE0 then
                        let b = int <| this.GetByte ()
                        let c = int <| this.GetByte ()
                        if ((b &&& 0xC0) <> 0x80) || ((c &&& 0xC0) <> 0x80) then failwith "MUTF-8: Bad second or third byte"
                        Array.push out (((a &&& 0x0F) <<< 12) ||| ((b &&& 0x3F) <<< 6) ||| (c &&& 0x3F))
                    else
                        failwith "MUTF-8: Bad byte"
            String.fromIntArray out
