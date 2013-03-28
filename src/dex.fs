namespace Dalvik

module Dex =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<Inline "String.fromCharCode.apply(null, $bs)">]
    let charsToString (bs : int array) : string = X<_> // HACK: `System.Text.Encoding` is not working =(

    [<Inline "$arr.push($x)">]
    let arrayPush (arr : 'a array) (x : 'a) : unit = X<_>

    
    [<JavaScript>]
    type DexFileArray [<JavaScript>] (data : Uint8Array) =
        let mutable offset = 0u

        let getByte () =
            let r = data.Get (uint64 offset)
            offset <- offset + 1u
            r
        member this.GetBytes (count : int) : byte array =
            Array.init count (fun _ -> getByte())

        member this.Length with get () = data.Length
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
        member val Strings : string array = [| |]
        member val Types : Type array = [| |]
        member val Protos : Proto array = [| |]
        member val Fields : Field array = [| |]
        member val Methods : Method array = [| |]
        member val Classes : Class array = [| |]

        static member Read (bytes : Uint8Array) =
            let dexf = new DexFile ()
            let stream = DexFileArray bytes

            let DEX_FILE_MAGIC = [| 0x64uy; 0x65uy; 0x78uy; 0x0auy; 0x30uy; 0x33uy; 0x35uy; 0x00uy; |]
            let ENDIAN_CONSTANT = 0x12345678u
            let NO_INDEX = 0xffffffff

            // Reading header
            stream.Seek 0u |> ignore
            Array.iter (fun b ->
                if (stream.GetByte() <> b) then failwith "Invalid DEX signature") DEX_FILE_MAGIC
            let checksum = stream.GetUInt32 ()
            let signature = stream.GetBytes 20
            let file_size = stream.GetUInt32 ()
            let header_size = stream.GetUInt32 ()
            if header_size <> 0x70u then failwith "Wrong header size"
            let endian_tag = stream.GetUInt32 ()
            if endian_tag <> ENDIAN_CONSTANT then failwith "Only little-endian files are supported"
            let link_size = stream.GetUInt32 ()
            if link_size <> 0u then failwith "Statically linked files are not supported"
            let link_off = stream.GetUInt32 ()
            if link_off <> 0u then failwith "link_off should be 0"
            let map_off = stream.GetUInt32 ()
            let string_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let type_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let proto_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let field_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let method_ids = (stream.GetUInt32 (), stream.GetUInt32 ())
            let class_defs = (stream.GetUInt32 (), stream.GetUInt32 ())
            let data = (stream.GetUInt32 (), stream.GetUInt32 ())

            DexFile.Read_string_ids stream string_ids dexf
            DexFile.Read_type_ids stream type_ids dexf
            DexFile.Read_proto_ids stream proto_ids dexf
            DexFile.Read_field_ids stream field_ids dexf
            DexFile.Read_method_ids stream method_ids dexf
            DexFile.Read_class_defs stream class_defs dexf

            dexf

        static member private Read_string_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let string_data_off = stream.GetUInt32 ()
                let prev_off = stream.Seek string_data_off
                let utf16_size = stream.GetULeb128 ()
                arrayPush dexf.Strings <| stream.GetMUTF8String ()
                stream.Seek prev_off |> ignore

        static member private Read_type_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let descriptor_idx = stream.GetUInt32 ()
                arrayPush dexf.Types <| new Type(dexf, descriptor_idx)

        static member private Read_proto_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let shorty_idx = stream.GetUInt32 ()
                let return_type_idx = stream.GetUInt32 ()
                let parameters_off = stream.GetUInt32 ()
                arrayPush dexf.Protos <| new Proto(dexf, shorty_idx, dexf.Types.[int32 return_type_idx],
                                                   Array.map (fun i -> dexf.Types.[int32 i]) <| DexFile.Read_type_list stream parameters_off)

        static member private Read_field_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let class_idx = stream.GetUInt16 ()
                let type_idx = stream.GetUInt16 ()
                let name_idx = stream.GetUInt32 ()
                arrayPush dexf.Fields <| new Field(dexf, dexf.Types.[int32 class_idx], dexf.Types.[int32 type_idx], name_idx)

        static member private Read_method_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let class_idx = stream.GetUInt16 ()
                let proto_idx = stream.GetUInt16 ()
                let name_idx = stream.GetUInt32 ()
                arrayPush dexf.Methods <| new Method(dexf, dexf.Types.[int32 class_idx], dexf.Protos.[int32 proto_idx], name_idx)

        static member private Read_class_defs stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let class_idx = stream.GetUInt32 ()
                let access_flags = stream.GetUInt32 ()
                let superclass_idx = stream.GetUInt32 ()
                let interfaces_off = stream.GetUInt32 ()
                let source_file_idx = stream.GetUInt32 ()
                let annotations_off = stream.GetUInt32 ()
                let class_data_off = stream.GetUInt32 ()
                let static_values_off = stream.GetUInt32 ()
                arrayPush dexf.Classes <| new Class(dexf, dexf.Types.[int32 class_idx], access_flags,
                                                    (if superclass_idx = 0u then None else Some dexf.Types.[int32 superclass_idx]),
                                                    Array.map (fun i -> dexf.Types.[int32 i]) <| DexFile.Read_type_list stream interfaces_off,
                                                    source_file_idx, (*annotations smth,*) class_data_off, static_values_off)

        static member private Read_type_list stream offset =
            if offset = 0u then [| |] else
            let oldpos = stream.Seek offset
            let size = stream.GetUInt32 ()
            let result : uint16 array = [| |]
            for i in {1..(int32 size)} do
                let type_idx = stream.GetUInt16 ()
                arrayPush result type_idx
            stream.Seek oldpos |> ignore
            result

    and
     [<JavaScript>]
     Type (dexf : DexFile, descriptor_idx : uint32) =
        member this.Descriptor with get () = dexf.Strings.[int32 descriptor_idx]
        override this.ToString () = this.Descriptor

    and
     [<JavaScript>]
     Proto (dexf : DexFile, shorty_idx : uint32, return_type : Type, parameters : Type array) =
        member this.Shorty with get () = dexf.Strings.[int32 shorty_idx]
        member this.ReturnType with get () = return_type
        member this.Parameters with get () = parameters
        override this.ToString () =
            "(" + String.concat ", " (Array.map (fun t -> t.ToString ()) this.Parameters) + ") -> " + this.ReturnType.ToString () 
    and
     [<JavaScript>]
     Field (dexf : DexFile, dclass : Type, dtype : Type, name_idx : uint32) =
        member this.Class with get () = dclass
        member this.Type with get () = dtype
        member this.Name with get () = dexf.Strings.[int32 name_idx]
        override this.ToString () =
            this.Name + " : " + this.Type.ToString()
    and
     [<JavaScript>]
     Method (dexf : DexFile, dclass : Type, proto : Proto, name_idx : uint32) =
        member this.Class with get () = dclass
        member this.Proto with get () = proto
        member this.Name with get () = dexf.Strings.[int32 name_idx]
        override this.ToString () =
            this.Name + " : " + this.Proto.ToString()
    and
     [<JavaScript>]
     Class (dexf : DexFile, dclass : Type, access_flags : uint32, superclass : Type option, interfaces : Type array,
            source_file_idx : uint32, (*annotations : ?,*) class_data_off : uint32, static_values_off : uint32) =
        member this.Type with get () = dclass
        (* access_flags getters *)
        member this.Super with get () = superclass
        member this.Interfaces with get () = interfaces
        member this.SourceFile with get () = dexf.Strings.[int32 source_file_idx]
        (* annotations *)
        (* class data *)
        (* static values *)
        override this.ToString () =
            "class " + this.Type.ToString () 