namespace Dalvik

module DexLoader =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    open Dex

    [<JavaScript>]
    module OpFormat =
        let read10x (stream : FileArray.DexFileArray) : unit =
            stream.GetByte () |> ignore

        let read12x (stream : FileArray.DexFileArray) : reg * reg =
            let ba = stream.GetByte ()
            (reg << nibble <| ba, reg << nibble <| (ba >>> 4))
        let read11n (stream : FileArray.DexFileArray) : reg * nibble =
            let ba = stream.GetByte ()
            (reg << nibble <| ba, nibble <| (ba >>> 4))

        let read11x (stream : FileArray.DexFileArray) : reg =
            reg <| stream.GetByte ()
        let read10t (stream : FileArray.DexFileArray) : int8 =
            stream.GetInt8 ()

        let read20t (stream : FileArray.DexFileArray) : int16 =
            stream.GetByte () |> ignore
            stream.GetInt16 ()

        let read22x (stream : FileArray.DexFileArray) : reg * reg =
            (reg <| stream.GetByte (), reg <| stream.GetUInt16 ())
        let read21t (stream : FileArray.DexFileArray) : reg * int16 =
            (reg <| stream.GetByte (), stream.GetInt16 ())
        let read21s (stream : FileArray.DexFileArray) : reg * int16 =
            (reg <| stream.GetByte (), stream.GetInt16 ())
        let read21h (stream : FileArray.DexFileArray) : reg * int16 =
            (reg <| stream.GetByte (), stream.GetInt16 ())
        let read21c (stream : FileArray.DexFileArray) : reg * uint16 =
            (reg <| stream.GetByte (), stream.GetUInt16 ())

        let read23x (stream : FileArray.DexFileArray) : reg * reg * reg =
            (reg <| stream.GetByte (), reg <| stream.GetByte (), reg <| stream.GetByte ())
        let read22b (stream : FileArray.DexFileArray) : reg * reg * int8 =
            (reg <| stream.GetByte (), reg <| stream.GetByte (), stream.GetInt8 ())

        let read22t (stream : FileArray.DexFileArray) : reg * reg * int16 =
            let ba = stream.GetByte ()
            (reg << unibble <| ba, reg << unibble <| (ba >>> 4), stream.GetInt16 ())
        let read22s (stream : FileArray.DexFileArray) : reg * reg * int16 =
            let ba = stream.GetByte ()
            (reg << unibble <| ba, reg << unibble <| (ba >>> 4), stream.GetInt16 ())
        let read22c (stream : FileArray.DexFileArray) : reg * reg * uint16 =
            let ba = stream.GetByte ()
            (reg << unibble <| ba, reg << unibble <| (ba >>> 4), stream.GetUInt16 ())

        let read30t (stream : FileArray.DexFileArray) : int32 =
            stream.GetInt32 ()

        let read32x (stream : FileArray.DexFileArray) : reg * reg =
            (reg <| stream.GetUInt16 (), reg <| stream.GetUInt16 ())

        let read31i (stream : FileArray.DexFileArray) : reg * int32 =
            (reg <| stream.GetByte (), stream.GetInt32 ())
        let read31t (stream : FileArray.DexFileArray) : reg * int32 =
            (reg <| stream.GetByte (), stream.GetInt32 ())
        let read31c (stream : FileArray.DexFileArray) : reg * uint32 =
            (reg <| stream.GetByte (), stream.GetUInt32 ())

        let read35c (stream : FileArray.DexFileArray) : unibble * uint16 * reg * reg * reg * reg * reg =
            let ag = stream.GetByte ()
            let meth = stream.GetUInt16 ()
            let fe = stream.GetByte ()
            let dc = stream.GetByte ()
            (unibble <| (ag >>> 4), meth, reg <| unibble dc, reg << unibble <| (dc >>> 4), reg <| unibble fe, reg << unibble <| (fe >>> 4), reg <| unibble ag)

        let read3rc (stream : FileArray.DexFileArray) : uint8 * uint16 * reg =
            (stream.GetByte (), stream.GetUInt16 (), reg <| stream.GetUInt16 ())

        let read51l (stream : FileArray.DexFileArray) : reg * GLong =
            (reg <| stream.GetByte (), stream.GetInt64 ())


    open OpFormat

    [<JavaScript>]
    let convert2addr (df, s) = (df, df, s)

    // FS0452, so it's here. TODO: move elsewhere.
    [<JavaScript>]
    let NO_INDEX = 0xFFFFFFFFu

    [<JavaScript>]
    type DexFile [<JavaScript>] private () =
        member val Strings : string array = [| |]
        member val Types : Type array = [| |]
        member val Protos : Proto array = [| |]
        member val Fields : Field array = [| |]
        member val Methods : Method array = [| |]
        member val Classes : Class array = [| |]

        static member Read (bytes : ArrayBuffer) (registerClass : Class -> unit) =
            let dexf = new DexFile ()
            let stream = FileArray.DexFileArray bytes

            let DEX_FILE_MAGIC = [| 0x64uy; 0x65uy; 0x78uy; 0x0Auy; 0x30uy; 0x33uy; 0x35uy; 0x00uy; |]
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
            DexFile.Read_class_defs stream class_defs dexf registerClass

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
                Array.push dexf.Types <| Type.Mew(dexf.Strings.[int32 descriptor_idx])

        static member private Read_proto_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let shorty_idx = stream.GetUInt32 ()
                let return_type_idx = stream.GetUInt32 ()
                let parameters_off = stream.GetUInt32 ()
                Array.push dexf.Protos <| Proto.Mew(dexf.Strings.[int32 shorty_idx], dexf.Types.[int32 return_type_idx],
                                                    Array.map (fun i -> dexf.Types.[int32 i]) <| DexFile.Read_type_list stream parameters_off)

        static member private Read_field_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let class_idx = stream.GetUInt16 ()
                let type_idx = stream.GetUInt16 ()
                let name_idx = stream.GetUInt32 ()
                let f = Field()
                Array.push dexf.Fields <| Field.Mew(dexf.Types.[int32 class_idx], dexf.Types.[int32 type_idx], dexf.Strings.[int32 name_idx])

        static member private Read_method_ids stream (size, offset) dexf =
            stream.Seek offset |> ignore
            for i in {1..(int32 size)} do
                let class_idx = stream.GetUInt16 ()
                let proto_idx = stream.GetUInt16 ()
                let name_idx = stream.GetUInt32 ()
                Array.push dexf.Methods <| Method.Mew(dexf.Types.[int32 class_idx], dexf.Protos.[int32 proto_idx], dexf.Strings.[int32 name_idx])

        static member private Read_class_defs stream (size, offset) dexf registerClass =
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

                let next_class_off = stream.Seek annotations_off
                //TODO #5 (read annotations here)

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
                        field.access_flags <- access_flags
                        Array.push arr field

                let encoded_methods (count : uint32) (arr : Method array) =
                    let mutable method_idx = 0u
                    for i in {1..(int32 count)} do
                        let method_idx_diff = stream.GetULeb128 ()
                        let access_flags = stream.GetULeb128 ()
                        let code_off = stream.GetULeb128 ()
                        method_idx <- method_idx + method_idx_diff
                        let meth = dexf.Methods.[int32 method_idx]
                        meth.access_flags <- access_flags
                        if code_off <> 0u then
                            let old_off = stream.Seek(code_off)
                            meth.registers_size <- stream.GetUInt16 ()
                            meth.ins_size <- stream.GetUInt16 ()
                            meth.outs_size <- stream.GetUInt16 ()
                            let tries_size = stream.GetUInt16 ()
                            let debug_info_off = stream.GetUInt32 ()
                            let insns_size = stream.GetUInt32 ()
                            meth.insns <- DexFile.Read_instructions stream insns_size dexf
                            if tries_size <> 0us && insns_size % 2u <> 0u then
                                stream.GetUInt16 () |> ignore
                            //TODO #10 (read tries and handlers)
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

                let c = Class.New(dexf.Types.[int class_idx], access_flags,
                                  (if superclass_idx = NO_INDEX then None else Some dexf.Types.[int32 superclass_idx]),
                                  Array.map (fun i -> dexf.Types.[int32 i]) <| DexFile.Read_type_list stream interfaces_off,
                                  (if source_file_idx = NO_INDEX then None else Some dexf.Strings.[int32 source_file_idx]),
                                  static_fields, instance_fields, direct_methods, virtual_methods,
                                  static_values)
                Array.push dexf.Classes c
                registerClass c
                stream.Seek next_class_off |> ignore

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
            let value_arg = (value_tag >>> 5) &&& 0x07uy
            let value_type = value_tag &&& 0x1Fuy
            match value_type with
                | 0x00uy -> Store.storeInt << int32 <| stream.GetByte ()
                | 0x02uy -> Store.storeInt << int32 <| stream.GetInt16Var (int value_arg + 1)
                | 0x03uy -> Store.storeInt << int32 <| stream.GetUInt16Var (int value_arg + 1)
                | 0x04uy -> Store.storeInt << int32 <| stream.GetInt32Var (int value_arg + 1)
                | 0x06uy -> Store.storeLong <| stream.GetInt64Var (int value_arg + 1)
                | 0x10uy -> Store.storeFloat <| stream.GetFloatVar (int value_arg + 1)
                | 0x11uy -> Store.storeDouble <| stream.GetDoubleVar (int value_arg + 1)
                | 0x17uy -> JsRef << As<obj> <| dexf.Strings.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x18uy -> JsRef << As<obj> <| dexf.Types.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x19uy -> JsRef << As<obj> <| dexf.Fields.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x1Auy -> JsRef << As<obj> <| dexf.Methods.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x1Buy -> JsRef << As<obj> <| dexf.Fields.[int <| stream.GetUInt32Var (int value_arg + 1)]
                | 0x1Cuy -> JsRef << As<obj> <| DexFile.Read_encoded_array stream dexf
                | 0x1Duy -> failwith "Annotations are not supported" //TODO #5
                | 0x1Euy -> JsRef null
                | 0x1Fuy -> Store.storeInt << int32 <| value_arg
                | _ -> failwith <| "Unsupported encoded_value type " + value_type.ToString ()

        static member private Read_instructions stream size dexf =
            let result : (uint32 * Instruction) array = [| |]
            let offset = ref 0u

            let readInstruction () =
                let op = stream.GetByte ()
                (match op with
                    | 0x00uy -> Nop << OpFormat.read10x
                    | 0x01uy
                    | 0x07uy -> Move << OpFormat.read12x
                    | 0x02uy
                    | 0x08uy -> Move << OpFormat.read22x
                    | 0x03uy
                    | 0x09uy -> Move << OpFormat.read32x
                    | 0x04uy -> MoveWide << OpFormat.read12x
                    | 0x05uy -> MoveWide << OpFormat.read22x
                    | 0x06uy -> MoveWide << OpFormat.read32x

                    | 0x0Auy
                    | 0x0Cuy -> MoveResult << OpFormat.read11x
                    | 0x0Buy -> MoveResultWide << OpFormat.read11x
                    | 0x0Duy -> MoveException << OpFormat.read11x

                    | 0x0Euy -> ReturnVoid << OpFormat.read10x
                    | 0x0Fuy
                    | 0x11uy -> Return << OpFormat.read11x
                    | 0x10uy -> ReturnWide << OpFormat.read11x

                    | 0x12uy -> Const4 << OpFormat.read11n
                    | 0x13uy -> Const16 << OpFormat.read21s
                    | 0x14uy -> Const << OpFormat.read31i
                    | 0x15uy -> ConstHigh16 << OpFormat.read21h
                    | 0x16uy -> ConstWide16 << OpFormat.read21s
                    | 0x17uy -> ConstWide32 << OpFormat.read31i
                    | 0x18uy -> ConstWide << OpFormat.read51l
                    | 0x19uy -> ConstWideHigh16 << OpFormat.read21h
                    | 0x1Auy -> ConstString << Arrows.secondOf2 (Array.get dexf.Strings << int) << OpFormat.read21c
                    | 0x1Buy -> ConstStringJumdo << Arrows.secondOf2 (Array.get dexf.Strings << int) << OpFormat.read31c
                    | 0x1Cuy -> ConstClass << Arrows.secondOf2 (Array.get dexf.Types << int) << OpFormat.read21c

                    | 0x1Duy -> MonitorEnter << OpFormat.read11x
                    | 0x1Euy -> MonitorExit << OpFormat.read11x

                    | 0x1Fuy -> CheckCast << Arrows.secondOf2 (Array.get dexf.Types << int) << OpFormat.read21c
                    | 0x20uy -> InstanceOf << Arrows.thirdOf3 (Array.get dexf.Types << int) << OpFormat.read22c
                    | 0x21uy -> ArrayLength << OpFormat.read12x
                    | 0x22uy -> NewInstance << Arrows.secondOf2 (Array.get dexf.Types << int) << OpFormat.read21c
                    | 0x23uy -> NewArray << Arrows.thirdOf3 (Array.get dexf.Types << int) << OpFormat.read22c
                    | 0x24uy -> FilledNewArray << Arrows.secondOf7 (Array.get dexf.Types << int) << OpFormat.read35c
                    | 0x25uy -> FilledNewArrayRange << Arrows.secondOf3 (Array.get dexf.Types << int) << OpFormat.read3rc
                    (*| 0x26uy -> let (r, b) = OpFormat.read31t
                                FillArrayData (reg r, ...read payload... *) //TODO #9

                    | 0x27uy -> Throw << OpFormat.read11x
                    | 0x28uy -> Goto << RelativeBytes << int32 << OpFormat.read10t
                    | 0x29uy -> Goto << RelativeBytes << int32 << OpFormat.read20t
                    | 0x2Auy -> Goto << RelativeBytes << int32 << OpFormat.read30t
                    (*| 0x2Buy -> *) //TODO #9 (packed-switch)
                    (*| 0x2Cuy -> *) //TODO #9 (sparse switch)

                    | 0x2Duy -> curry CmpFloat LtBias << OpFormat.read23x
                    | 0x2Euy -> curry CmpFloat GtBias << OpFormat.read23x
                    | 0x2Fuy -> curry CmpDouble LtBias << OpFormat.read23x
                    | 0x30uy -> curry CmpDouble GtBias << OpFormat.read23x
                    | 0x31uy -> CmpLong << OpFormat.read23x

                    | 0x32uy -> curry If Eq << Arrows.thirdOf3 (RelativeBytes << int32) << OpFormat.read22t
                    | 0x33uy -> curry If Ne << Arrows.thirdOf3 (RelativeBytes << int32) << OpFormat.read22t
                    | 0x34uy -> curry If Lt << Arrows.thirdOf3 (RelativeBytes << int32) << OpFormat.read22t
                    | 0x35uy -> curry If Ge << Arrows.thirdOf3 (RelativeBytes << int32) << OpFormat.read22t
                    | 0x36uy -> curry If Gt << Arrows.thirdOf3 (RelativeBytes << int32) << OpFormat.read22t
                    | 0x37uy -> curry If Le << Arrows.thirdOf3 (RelativeBytes << int32) << OpFormat.read22t
                    | 0x38uy -> curry IfZ Eq << Arrows.secondOf2 (RelativeBytes << int32) << OpFormat.read21t
                    | 0x39uy -> curry IfZ Ne << Arrows.secondOf2 (RelativeBytes << int32) << OpFormat.read21t
                    | 0x3Auy -> curry IfZ Lt << Arrows.secondOf2 (RelativeBytes << int32) << OpFormat.read21t
                    | 0x3Buy -> curry IfZ Ge << Arrows.secondOf2 (RelativeBytes << int32) << OpFormat.read21t
                    | 0x3Cuy -> curry IfZ Gt << Arrows.secondOf2 (RelativeBytes << int32) << OpFormat.read21t
                    | 0x3Duy -> curry IfZ Le << Arrows.secondOf2 (RelativeBytes << int32) << OpFormat.read21t

                    | 0x44uy
                    | 0x46uy
                    | 0x47uy
                    | 0x48uy
                    | 0x49uy
                    | 0x4Auy -> Aget << OpFormat.read23x
                    | 0x45uy -> AgetWide << OpFormat.read23x
                    | 0x4Buy
                    | 0x4Duy
                    | 0x4Euy
                    | 0x4Fuy
                    | 0x50uy
                    | 0x51uy -> Aput << OpFormat.read23x
                    | 0x4Cuy -> AputWide << OpFormat.read23x

                    | 0x52uy
                    | 0x54uy
                    | 0x55uy
                    | 0x56uy
                    | 0x57uy
                    | 0x58uy -> Iget << Arrows.thirdOf3 (Array.get dexf.Fields << int) << OpFormat.read22c
                    | 0x53uy -> IgetWide << Arrows.thirdOf3 (Array.get dexf.Fields << int) << OpFormat.read22c
                    | 0x59uy
                    | 0x5Buy
                    | 0x5Cuy
                    | 0x5Duy
                    | 0x5Euy
                    | 0x5Fuy -> Iput << Arrows.thirdOf3 (Array.get dexf.Fields << int) << OpFormat.read22c
                    | 0x5Auy -> IputWide << Arrows.thirdOf3 (Array.get dexf.Fields << int) << OpFormat.read22c

                    | 0x60uy
                    | 0x62uy
                    | 0x63uy
                    | 0x64uy
                    | 0x65uy
                    | 0x66uy -> Sget << Arrows.secondOf2 (Array.get dexf.Fields << int) << OpFormat.read21c
                    | 0x61uy -> SgetWide << Arrows.secondOf2 (Array.get dexf.Fields << int) << OpFormat.read21c
                    | 0x67uy
                    | 0x69uy
                    | 0x6Auy
                    | 0x6Buy
                    | 0x6Cuy
                    | 0x6Duy -> Sput << Arrows.secondOf2 (Array.get dexf.Fields << int) << OpFormat.read21c
                    | 0x68uy -> SputWide << Arrows.secondOf2 (Array.get dexf.Fields << int) << OpFormat.read21c

                    | 0x6Euy -> curry Invoke InvokeVirtual << Arrows.secondOf7 (Array.get dexf.Methods << int) << OpFormat.read35c
                    | 0x6Fuy -> curry Invoke InvokeSuper << Arrows.secondOf7 (Array.get dexf.Methods << int) << OpFormat.read35c
                    | 0x70uy -> curry Invoke InvokeDirect << Arrows.secondOf7 (Array.get dexf.Methods << int) << OpFormat.read35c
                    | 0x71uy -> curry Invoke InvokeStatic << Arrows.secondOf7 (Array.get dexf.Methods << int) << OpFormat.read35c
                    | 0x72uy -> curry Invoke InvokeInterface << Arrows.secondOf7 (Array.get dexf.Methods << int) << OpFormat.read35c
                    | 0x74uy -> curry InvokeRange InvokeVirtual << Arrows.secondOf3 (Array.get dexf.Methods << int) << OpFormat.read3rc
                    | 0x75uy -> curry InvokeRange InvokeSuper << Arrows.secondOf3 (Array.get dexf.Methods << int) << OpFormat.read3rc
                    | 0x76uy -> curry InvokeRange InvokeDirect << Arrows.secondOf3 (Array.get dexf.Methods << int) << OpFormat.read3rc
                    | 0x77uy -> curry InvokeRange InvokeStatic << Arrows.secondOf3 (Array.get dexf.Methods << int) << OpFormat.read3rc
                    | 0x78uy -> curry InvokeRange InvokeInterface << Arrows.secondOf3 (Array.get dexf.Methods << int) << OpFormat.read3rc

                    | 0x7Buy -> NegInt << OpFormat.read12x
                    | 0x7Cuy -> NotInt << OpFormat.read12x
                    | 0x7Duy -> NegLong << OpFormat.read12x
                    | 0x7Euy -> NotLong << OpFormat.read12x
                    | 0x7Fuy -> NegFloat << OpFormat.read12x
                    | 0x80uy -> NegDouble << OpFormat.read12x
                    | 0x81uy -> IntToLong << OpFormat.read12x
                    | 0x82uy -> IntToFloat << OpFormat.read12x
                    | 0x83uy -> IntToDouble << OpFormat.read12x
                    | 0x84uy -> LongToInt << OpFormat.read12x
                    | 0x85uy -> LongToFloat << OpFormat.read12x
                    | 0x86uy -> LongToDouble << OpFormat.read12x
                    | 0x87uy -> FloatToInt << OpFormat.read12x
                    | 0x88uy -> FloatToLong << OpFormat.read12x
                    | 0x89uy -> FloatToDouble << OpFormat.read12x
                    | 0x8Auy -> DoubleToInt << OpFormat.read12x
                    | 0x8Buy -> DoubleToLong << OpFormat.read12x
                    | 0x8Cuy -> DoubleToFloat << OpFormat.read12x
                    | 0x8Duy -> IntToByte << OpFormat.read12x
                    | 0x8Euy -> IntToChar << OpFormat.read12x
                    | 0x8Fuy -> IntToShort << OpFormat.read12x

                    | 0x90uy -> AddInt  << OpFormat.read23x
                    | 0x91uy -> SubInt  << OpFormat.read23x
                    | 0x92uy -> MulInt  << OpFormat.read23x
                    | 0x93uy -> DivInt  << OpFormat.read23x
                    | 0x94uy -> RemInt  << OpFormat.read23x
                    | 0x95uy -> AndInt  << OpFormat.read23x
                    | 0x96uy -> OrInt   << OpFormat.read23x
                    | 0x97uy -> XorInt  << OpFormat.read23x
                    | 0x98uy -> ShlInt  << OpFormat.read23x
                    | 0x99uy -> ShrInt  << OpFormat.read23x
                    | 0x9Auy -> UshrInt << OpFormat.read23x
                    | 0x9Buy -> AddLong  << OpFormat.read23x
                    | 0x9Cuy -> SubLong  << OpFormat.read23x
                    | 0x9Duy -> MulLong  << OpFormat.read23x
                    | 0x9Euy -> DivLong  << OpFormat.read23x
                    | 0x9Fuy -> RemLong  << OpFormat.read23x
                    | 0xA0uy -> AndLong  << OpFormat.read23x
                    | 0xA1uy -> OrLong   << OpFormat.read23x
                    | 0xA2uy -> XorLong  << OpFormat.read23x
                    | 0xA3uy -> ShlLong  << OpFormat.read23x
                    | 0xA4uy -> ShrLong  << OpFormat.read23x
                    | 0xA5uy -> UshrLong << OpFormat.read23x
                    | 0xA6uy -> AddFloat << OpFormat.read23x
                    | 0xA7uy -> SubFloat << OpFormat.read23x
                    | 0xA8uy -> MulFloat << OpFormat.read23x
                    | 0xA9uy -> DivFloat << OpFormat.read23x
                    | 0xAAuy -> RemFloat << OpFormat.read23x
                    | 0xABuy -> AddDouble << OpFormat.read23x
                    | 0xACuy -> SubDouble << OpFormat.read23x
                    | 0xADuy -> MulDouble << OpFormat.read23x
                    | 0xAEuy -> DivDouble << OpFormat.read23x
                    | 0xAFuy -> RemDouble << OpFormat.read23x

                    | 0xB0uy -> AddInt  << convert2addr << OpFormat.read12x
                    | 0xB1uy -> SubInt  << convert2addr << OpFormat.read12x
                    | 0xB2uy -> MulInt  << convert2addr << OpFormat.read12x
                    | 0xB3uy -> DivInt  << convert2addr << OpFormat.read12x
                    | 0xB4uy -> RemInt  << convert2addr << OpFormat.read12x
                    | 0xB5uy -> AndInt  << convert2addr << OpFormat.read12x
                    | 0xB6uy -> OrInt   << convert2addr << OpFormat.read12x
                    | 0xB7uy -> XorInt  << convert2addr << OpFormat.read12x
                    | 0xB8uy -> ShlInt  << convert2addr << OpFormat.read12x
                    | 0xB9uy -> ShrInt  << convert2addr << OpFormat.read12x
                    | 0xBAuy -> UshrInt << convert2addr << OpFormat.read12x
                    | 0xBBuy -> AddLong  << convert2addr << OpFormat.read12x
                    | 0xBCuy -> SubLong  << convert2addr << OpFormat.read12x
                    | 0xBDuy -> MulLong  << convert2addr << OpFormat.read12x
                    | 0xBEuy -> DivLong  << convert2addr << OpFormat.read12x
                    | 0xBFuy -> RemLong  << convert2addr << OpFormat.read12x
                    | 0xC0uy -> AndLong  << convert2addr << OpFormat.read12x
                    | 0xC1uy -> OrLong   << convert2addr << OpFormat.read12x
                    | 0xC2uy -> XorLong  << convert2addr << OpFormat.read12x
                    | 0xC3uy -> ShlLong  << convert2addr << OpFormat.read12x
                    | 0xC4uy -> ShrLong  << convert2addr << OpFormat.read12x
                    | 0xC5uy -> UshrLong << convert2addr << OpFormat.read12x
                    | 0xC6uy -> AddFloat << convert2addr << OpFormat.read12x
                    | 0xC7uy -> SubFloat << convert2addr << OpFormat.read12x
                    | 0xC8uy -> MulFloat << convert2addr << OpFormat.read12x
                    | 0xC9uy -> DivFloat << convert2addr << OpFormat.read12x
                    | 0xCAuy -> RemFloat << convert2addr << OpFormat.read12x
                    | 0xCBuy -> AddDouble << convert2addr << OpFormat.read12x
                    | 0xCCuy -> SubDouble << convert2addr << OpFormat.read12x
                    | 0xCDuy -> MulDouble << convert2addr << OpFormat.read12x
                    | 0xCEuy -> DivDouble << convert2addr << OpFormat.read12x
                    | 0xCFuy -> RemDouble << convert2addr << OpFormat.read12x

                    | 0xD0uy -> AddIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22s
                    | 0xD1uy -> RsubIntLit << Arrows.thirdOf3 int32 << OpFormat.read22s
                    | 0xD2uy -> MulIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22s
                    | 0xD3uy -> DivIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22s
                    | 0xD4uy -> RemIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22s
                    | 0xD5uy -> AndIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22s
                    | 0xD6uy -> OrIntLit   << Arrows.thirdOf3 int32 << OpFormat.read22s
                    | 0xD7uy -> XorIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22s

                    | 0xD8uy -> AddIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22b
                    | 0xD9uy -> RsubIntLit << Arrows.thirdOf3 int32 << OpFormat.read22b
                    | 0xDAuy -> MulIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22b
                    | 0xDBuy -> DivIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22b
                    | 0xDCuy -> RemIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22b
                    | 0xDDuy -> AndIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22b
                    | 0xDEuy -> OrIntLit   << Arrows.thirdOf3 int32 << OpFormat.read22b
                    | 0xDFuy -> XorIntLit  << Arrows.thirdOf3 int32 << OpFormat.read22b

                    | _      -> failwith <| "Instruction not implemented " + op.ToString ()
                ) stream

            let resolveOffset (cur, insn) = // TODO: more efficient lookup?
                let resolved rel =
                    match Array.tryFindIndex (fun (ofs, _) -> int32 ofs = int32 cur + rel) result with
                        | Some i -> AbsoluteIndex i
                        | None -> failwith "Could not resolve relative offset"
                    
                match insn with
                   (*| FillArrayData ... -> *) //TODO #9 [Why is it here? It should't contain an offset, the payload should already be read]
                   | Goto (RelativeBytes off)               -> Goto (resolved off)
                   | If (test, (a, b, RelativeBytes off))   -> If (test, (a, b, resolved off))
                   | IfZ (test, (a, RelativeBytes off))     -> IfZ (test, (a, resolved off))
                   | _                                      -> insn
                

            while !offset < size do
                let pos = stream.Offset
                Array.push result <| (!offset, readInstruction ())
                offset := !offset + (stream.Offset - pos) / 2u
            Array.map resolveOffset result