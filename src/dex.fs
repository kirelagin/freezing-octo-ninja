namespace Dalvik

module Dex =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5
    open ByteCode

    // FS0452, so it's here. TODO: move elsewhere.
    [<JavaScript>]
    let NO_INDEX = 0xffffffffu

    [<JavaScript>]
    type DexFile [<JavaScript>] private () =
        member val Strings : string array = [| |]
        member val Types : Type array = [| |]
        member val Protos : Proto array = [| |]
        member val Fields : Field array = [| |]
        member val Methods : Method array = [| |]
        member val Classes : Class array = [| |]

        static member Read (bytes : ArrayBuffer) =
            let dexf = new DexFile ()
            let stream = FileArray.DexFileArray bytes

            let DEX_FILE_MAGIC = [| 0x64uy; 0x65uy; 0x78uy; 0x0auy; 0x30uy; 0x33uy; 0x35uy; 0x00uy; |]
            let ENDIAN_CONSTANT = 0x12345678u

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
                Array.push dexf.Strings <| stream.GetMUTF8String ()
                stream.Seek prev_off |> ignore

        static member private Read_type_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let descriptor_idx = stream.GetUInt32 ()
                Array.push dexf.Types <| new Type(dexf.Strings.[int32 descriptor_idx])

        static member private Read_proto_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let shorty_idx = stream.GetUInt32 ()
                let return_type_idx = stream.GetUInt32 ()
                let parameters_off = stream.GetUInt32 ()
                Array.push dexf.Protos <| new Proto(dexf.Strings.[int32 shorty_idx], dexf.Types.[int32 return_type_idx],
                                                   Array.map (fun i -> dexf.Types.[int32 i]) <| DexFile.Read_type_list stream parameters_off)

        static member private Read_field_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let class_idx = stream.GetUInt16 ()
                let type_idx = stream.GetUInt16 ()
                let name_idx = stream.GetUInt32 ()
                Array.push dexf.Fields <| new Field(dexf.Types.[int32 class_idx], dexf.Types.[int32 type_idx], dexf.Strings.[int32 name_idx])

        static member private Read_method_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let class_idx = stream.GetUInt16 ()
                let proto_idx = stream.GetUInt16 ()
                let name_idx = stream.GetUInt32 ()
                Array.push dexf.Methods <| new Method(dexf.Types.[int32 class_idx], dexf.Protos.[int32 proto_idx], dexf.Strings.[int32 name_idx])

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

                // Reading class_data
                stream.Seek class_data_off |> ignore

                let static_fields : Field array = [| |]
                let instance_fields : Field array = [| |]
                let direct_methods : Method array = [| |]
                let virtual_methods : Method array= [| |]

                let static_fields_size = stream.GetULeb128 ()
                let instance_fields_size = stream.GetULeb128 ()
                let direct_methods_size = stream.GetULeb128 ()
                let virtual_methods_size = stream.GetULeb128 ()

                let encoded_fields (count : uint32) (arr : Field array) =
                    let mutable field_idx = 0u
                    for i in {1..(int32 count)} do
                        let field_idx_diff = stream.GetULeb128 ()
                        let access_flags = stream.GetULeb128 ()
                        field_idx <- field_idx + field_idx_diff
                        let field = dexf.Fields.[int32 field_idx]
                        field.AccessFlags <- access_flags
                        Array.push arr field

                let encoded_methods (count : uint32) (arr : Method array) =
                    let mutable method_idx = 0u
                    for i in {1..(int32 count)} do
                        let method_idx_diff = stream.GetULeb128 ()
                        let access_flags = stream.GetULeb128 ()
                        let code_off = stream.GetULeb128 ()
                        method_idx <- method_idx + method_idx_diff
                        let meth = dexf.Methods.[int32 method_idx]
                        meth.AccessFlags <- access_flags
                        if code_off <> 0u then
                            let old_off = stream.Seek(code_off)
                            meth.RegistersSize <- stream.GetUInt16 ()
                            meth.InsSize <- stream.GetUInt16 ()
                            meth.OutsSize <- stream.GetUInt16 ()
                            let tries_size = stream.GetUInt16 ()
                            let debug_info_off = stream.GetUInt32 ()
                            let insns_size = stream.GetUInt32 ()
                            meth.Insns <- DexFile.Read_instructions stream insns_size dexf
                            if tries_size <> 0us && insns_size % 2u <> 0u then
                                stream.GetUInt16 () |> ignore
                            // TODO: read tries and handlers
                            stream.Seek old_off |> ignore
                        Array.push arr meth
                
                encoded_fields static_fields_size static_fields
                encoded_fields instance_fields_size instance_fields
                encoded_methods direct_methods_size direct_methods
                encoded_methods virtual_methods_size virtual_methods

                // Reading static values
                let static_values =
                    if static_values_off <> 0u then
                        stream.Seek static_values_off |> ignore
                        DexFile.Read_encoded_array stream dexf
                    else
                        [| |]

                Array.push dexf.Classes <| new Class(dexf.Types.[int32 class_idx], access_flags,
                                                    (if superclass_idx = NO_INDEX then None else Some dexf.Types.[int32 superclass_idx]),
                                                    Array.map (fun i -> dexf.Types.[int32 i]) <| DexFile.Read_type_list stream interfaces_off,
                                                    (if source_file_idx = NO_INDEX then None else Some dexf.Strings.[int32 source_file_idx]),
                                                    (* TODO: annotations smth,*)
                                                    static_fields, instance_fields, direct_methods, virtual_methods,
                                                    static_values)

        static member private Read_type_list stream offset =
            if offset = 0u then [| |] else
            let oldpos = stream.Seek offset
            let size = stream.GetUInt32 ()
            let result : uint16 array = [| |]
            for i in {1..(int32 size)} do
                let type_idx = stream.GetUInt16 ()
                Array.push result type_idx
            stream.Seek oldpos |> ignore
            result

        static member private Read_encoded_array stream dexf =
            let size = stream.GetULeb128 ()
            Array.init (int size) (fun _ -> DexFile.Read_encoded_value stream dexf)

        static member private Read_encoded_value stream dexf =
            let value_tag = stream.GetByte ()
            let value_arg = (value_tag >>> 5) &&& 0x7uy
            let value_type = value_tag &&& 0x1uy
            match value_type with
                | 0x00uy -> JsNumber << float64 <| stream.GetByte ()
                | 0x02uy -> JsNumber << float64 <| stream.GetInt16Var (int value_arg + 1)
                | 0x03uy -> JsNumber << float64 <| stream.GetUInt16Var (int value_arg + 1)
                | 0x04uy -> JsNumber << float64 <| stream.GetInt32Var (int value_arg + 1)
                | 0x06uy -> JsLong << GLong.FromBits <| stream.GetInt64Var (int value_arg + 1)
                | 0x10uy -> JsNumber << float64 <| stream.GetFloatVar (int value_arg + 1)
                | 0x11uy -> JsNumber << float64 <| stream.GetDoubleVar (int value_arg + 1)
                | 0x17uy -> JsRef << As<obj> <| dexf.Strings.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x18uy -> JsRef << As<obj> <| dexf.Types.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x19uy -> JsRef << As<obj> <| dexf.Fields.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x1Auy -> JsRef << As<obj> <| dexf.Methods.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x1Buy -> JsRef << As<obj> <| dexf.Fields.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x1Cuy -> JsRef << As<obj> <| DexFile.Read_encoded_array stream dexf
                | 0x1duy -> failwith "Annotations are not supported" // TODO: annotations
                | 0x1Euy -> JsRef null
                | 0x1Fuy -> JsNumber << float64 <| value_arg
                | _ -> failwith <| "Unsupported encoded_value type " + value_type.ToString ()

        static member private Read_instructions stream size dexf =
            let result : Instruction array = [| |]
            let offset = ref 0u

            let readInstruction () =
                let op = stream.GetByte ()
                match op with
                    | 0x00uy -> stream.GetByte () |> ignore
                                Nop
                    | 0x0Euy -> stream.GetByte () |> ignore
                                ReturnVoid
                    | 0x0Fuy -> Return (reg <| stream.GetByte ())
                    | 0x14uy -> Const (reg <| stream.GetByte (), stream.GetInt32 ())
                    | 0x18uy -> ConstWide (reg <| stream.GetByte (), stream.GetInt64 ())
                    | 0x28uy -> Goto <| Unresolved (!offset, int32 <| stream.GetByte ())
                    | 0x21uy -> CmpLong (reg <| stream.GetByte (), reg <| stream.GetByte (), reg <| stream.GetByte ())
                    | 0x33uy -> let regs = stream.GetByte ()
                                If (Ne, reg <| nibble regs, reg <| nibble (regs >>> 4), Unresolved (!offset, stream.GetInt16 ()))
                    | 0x39uy -> IfZ (Ne, reg <| stream.GetByte (), Unresolved (!offset, stream.GetInt16 ()))
                    | 0x70uy -> let ag = stream.GetByte ()
                                let meth = stream.GetUInt16 ()
                                let fe = stream.GetByte ()
                                let dc = stream.GetByte ()
                                Invoke (InvokeDirect, nibble <| ag >>> 4, meth, reg <| nibble dc, reg << nibble <| dc >>> 4, reg <| nibble fe, reg << nibble <| fe >>> 4, reg <| nibble ag)
                    | _      -> failwith <| "Instruction not implemented " + op.ToString ()

            while !offset < size do
                let pos = stream.Offset
                Array.push result <| readInstruction ()
                offset := !offset + (stream.Offset - pos) / 2u
            result


    and
     [<JavaScript>]
     Type (descriptor : string) =
        member this.Descriptor = descriptor
        override this.ToString () = descriptor

    and
     [<JavaScript>]
     Proto (shorty : string, return_type : Type, parameters : Type array) =
        member this.Shorty = shorty
        member this.ReturnType = return_type
        member this.Parameters = parameters
        override this.ToString () =
            "(" + String.concat ", " (Array.map (fun t -> t.ToString ()) parameters) + ") -> " + return_type.ToString () 
    and
     [<JavaScript>]
     Field (dclass : Type, dtype : Type, name : string) =
        member this.Class = dclass
        member this.Type = dtype
        member this.Name = name
        member val AccessFlags = 0u with get, set
        override this.ToString () =
            name + " : " + dclass.ToString()
    and
     [<JavaScript>]
     Method (dclass : Type, proto : Proto, name : string) =
        member this.Class = dclass
        member this.Proto = proto
        member this.Name = name
        member val AccessFlags = 0u with get, set
        member val RegistersSize = 0us with get, set
        member val InsSize = 0us with get, set
        member val OutsSize = 0us with get, set
        member val Insns : Instruction array = [| |] with get, set
        override this.ToString () =
            name + " : " + proto.ToString()
    and
     [<JavaScript>]
     Class (dclass : Type, access_flags : uint32, superclass : Type option, interfaces : Type array,
            source_file : string option, (* TODO: annotations : ?,*)
            static_fields : Field array, instance_fields : Field array, direct_methods : Method array, virual_methods : Method array,
            static_values : JsValue array) =
        member this.Class = dclass
        (* TODO: access_flags getters *)
        member this.Super = superclass
        member this.Interfaces = interfaces
        member this.SourceFile = source_file
        (* TODO: annotations *)
        // TODO: static_values seems to be never used in the real world… hm-hm-hm…
        override this.ToString () =
            "class " + dclass.ToString ()
