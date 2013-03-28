namespace Dalvik

module FileArray =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<Inline "String.fromCharCode.apply(null, $bs)">]
    let charsToString (bs : int array) : string = X<_> // HACK: `System.Text.Encoding` is not working =(

    [<JavaScript>]
    type FileArray (data : Uint8Array) =
        let mutable offset = 0u

        let getByte () =
            let r = data.Get (uint64 offset)
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
            let d = this.GetBytes 2
            (uint16 d.[1] * 256us) ||| (uint16 d.[0])
        member this.GetUInt32 () : uint32 =
            let d = this.GetBytes 4
            (uint32 d.[3] * 0x1000000u) ||| (uint32 d.[2] * 0x10000u) ||| (uint32 d.[1] * 0x100u) ||| (uint32 d.[0])

    [<JavaScript>]
    type DexFileArray (data : Uint8Array) =
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