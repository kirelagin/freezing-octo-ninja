namespace Dalvik

module Dex =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<Inline "$data[$i]">]
    let get (data : Uint8Array) (i : int) = X<_> // HACK: this is to avoid WebSharper's integer conversion difficulties

    [<Inline "String.fromCharCode.apply(null, $bs)">]
    let charsToString (bs : int array) : string = X<_> // HACK: `System.Text.Encoding` is not working =(

    [<Inline "$arr.push($x)">]
    let arrayPush (arr : 'a array) (x : 'a) : unit = X<_>

    
    [<JavaScript>]
    type DexFileArray [<JavaScript>] (data : Uint8Array) =
        let mutable offset = 0

        let getByte () =
            let r = get data offset
            offset <- offset + 1
            r
        member this.GetBytes (count : int) : byte array =
            Array.init count (fun _ -> getByte())

        member this.Length with get () = data.Length
        member this.Seek (newOffset : int) =
            let old = offset
            offset <- newOffset
            old
        member this.GetByte () : byte =
            getByte ()
        member this.GetUInt16 () : int =
            let d = this.GetBytes 2
            (int d.[1] * 256) ||| (int d.[0])
        member this.GetUInt32 () : int =
            let d = this.GetBytes 4
            (int d.[3] * 0x1000000) ||| (int d.[2] * 0x10000) ||| (int d.[1] * 0x100) ||| (int d.[0])

        member private this.GetLeb128 (signed : bool) =
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
        member this.GetULeb128 () = Numbers.unsign <| this.GetLeb128 false
        member this.GetSLeb128 () = this.GetLeb128 true

        member this.GetMUTF8String () : string =
            let out : int array = [| |]
            let mutable stop = false
            while not stop do
                let a = int <| this.GetByte ()
                if a = 0 then
                    stop <- true
                else
                    if a < 0x80 then
                        arrayPush out a
                    else if (a &&& 0xe0) = 0xc0 then
                        let b = int <| this.GetByte ()
                        if ((b &&& 0xc0) <> 0x80) then failwith "MUTF-8: Bad second byte"
                        arrayPush out (((a &&& 0x1f) <<< 6) ||| (b &&& 0x3f))
                    else if (a &&& 0xf0) = 0xe0 then
                        let b = int <| this.GetByte ()
                        let c = int <| this.GetByte ()
                        if ((b &&& 0xc0) <> 0x80) || ((c &&& 0xc0) <> 0x80) then failwith "MUTF-8: Bad second or third byte"
                        arrayPush out (((a &&& 0x0f) <<< 12) ||| ((b &&& 0x3f) <<< 6) ||| (c &&& 0x3f))
                    else
                        failwith "MUTF-8: Bad byte"
            charsToString out


    [<JavaScript>]
    type DexFile [<JavaScript>] private () =
        member val strings : string array = [| |]
        member val types = new ResizeArray<string>()
        member val prototypes = new ResizeArray<string>()
        member val fields = new ResizeArray<string>()
        member val methods = new ResizeArray<string>()
        member val classes = new ResizeArray<string>()

        static member Read (bytes : Uint8Array) =
            let dexf = new DexFile ()
            let stream = DexFileArray bytes

            let DEX_FILE_MAGIC = [| 0x64uy; 0x65uy; 0x78uy; 0x0auy; 0x30uy; 0x33uy; 0x35uy; 0x00uy; |]
            let ENDIAN_CONSTANT = 0x12345678
            let NO_INDEX = 0xffffffff

            // Reading header
            stream.Seek 0 |> ignore
            Array.iter (fun b ->
                if (stream.GetByte() <> b) then failwith "Invalid DEX signature") DEX_FILE_MAGIC
            let checksum = stream.GetUInt32 ()
            let signature = stream.GetBytes 20
            let file_size = stream.GetUInt32 ()
            let header_size = stream.GetUInt32 ()
            if header_size <> 0x70 then failwith "Wrong header size"
            let endian_tag = stream.GetUInt32 ()
            if endian_tag <> ENDIAN_CONSTANT then failwith "Only little-endian files are supported"
            let link_size = stream.GetUInt32 ()
            if link_size <> 0 then failwith "Statically linked files are not supported"
            let link_off = stream.GetUInt32 ()
            if link_off <> 0 then failwith "link_off should be 0"
            let map_off = stream.GetUInt32 ()
            let string_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let type_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let proto_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let field_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let method_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let class_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let data = (stream.GetUInt32 (), stream.GetUInt32 ())

            DexFile.Read_string_ids stream string_ids dexf

            dexf

        static member private Read_string_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore

            for i in {0..(size-1)} do
                let string_data_off = stream.GetUInt32 ()
                let prev_off = stream.Seek string_data_off
                let utf16_size = stream.GetULeb128 ()
                arrayPush dexf.strings <| stream.GetMUTF8String ()