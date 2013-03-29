namespace Dalvik

module FileArray =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<Inline "String.fromCharCode.apply(null, $bs)">]
    let charsToString (bs : int array) : string = X<_> // HACK: `System.Text.Encoding` is not working =(

    [<Name [| "DataView" |]>]
    [<Stub>]
    type DataView (data : ArrayBuffer) =
        [<Name "getInt8">]
        member this.GetInt8(byteOffset : uint64) : int8 = X<_>
        [<Name "getUint8">]
        member this.GetUint8(byteOffset : uint64) : uint8 = X<_>
        [<Name "getInt16">]
        member this.GetInt16(byteOffset : uint64, littleEndian : bool) : int16 = X<_>
        [<Name "getUint16">]
        member this.GetUint16(byteOffset : uint64, littleEndian : bool) : uint16 = X<_>
        [<Name "getInt32">]
        member this.GetInt32(byteOffset : uint64, littleEndian : bool) : int32 = X<_>
        [<Name "getUint32">]
        member this.GetUint32(byteOffset : uint64, littleEndian : bool) : uint32 = X<_>
        [<Name "getFloat32">]
        member this.GetFloat32(byteOffset : uint64, littleEndian : bool) : float32 = X<_>
        [<Name "getFloat64">]
        member this.GetFloat64(byteOffset : uint64, littleEndian : bool) : double = X<_>

    [<JavaScript>]
    type FileArray (data : ArrayBuffer) =
        let mutable offset = 0u
        let view = new DataView(data)

        let getByte () : byte =
            let r = view.GetUint8 (uint64 offset)
            offset <- offset + 1u
            r
        member this.GetBytes (count : int) : byte array =
            Array.init count (fun _ -> getByte())

        member this.Length = data.Length
        member this.Seek (newOffset : uint32) =
            let old = offset
            offset <- newOffset
            old
        member this.GetByte () : byte =
            getByte ()
        member this.GetUInt16 () : uint16 =
            let r = view.GetUint16 (uint64 offset, true)
            offset <- offset + 2u
            r
        member this.GetUInt32 () : uint32 =
            let r = view.GetUint32 (uint64 offset, true)
            offset <- offset + 4u
            r

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
                result <- result ||| ((cur &&& 0x7f) <<< (count * 7))
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
                    else if (a &&& 0xe0) = 0xc0 then
                        let b = int <| this.GetByte ()
                        if ((b &&& 0xc0) <> 0x80) then failwith "MUTF-8: Bad second byte"
                        Array.push out (((a &&& 0x1f) <<< 6) ||| (b &&& 0x3f))
                    else if (a &&& 0xf0) = 0xe0 then
                        let b = int <| this.GetByte ()
                        let c = int <| this.GetByte ()
                        if ((b &&& 0xc0) <> 0x80) || ((c &&& 0xc0) <> 0x80) then failwith "MUTF-8: Bad second or third byte"
                        Array.push out (((a &&& 0x0f) <<< 12) ||| ((b &&& 0x3f) <<< 6) ||| (c &&& 0x3f))
                    else
                        failwith "MUTF-8: Bad byte"
            charsToString out
