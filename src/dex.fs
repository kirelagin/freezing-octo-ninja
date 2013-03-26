namespace Dalvik

module Dex =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5
    
    [<JavaScript>]
    type DexFileArray [<JavaScript>] (data : Uint8Array) =
        let mutable offset = 0UL

        let getByte () =
            let r = data.Get(offset)
            offset <- offset + 1UL
            r
        let getBytes (count : int) : byte array =
            Array.init count (fun _ -> getByte())
        let getConverted (count : int) convertor =
            convertor (Array.rev <| getBytes count, 0)

        member this.Length with get () = data.Length
        member this.Seek (newOffset : uint64) =
            offset <- newOffset
            data.Get(0UL)
        member this.GetByte () : byte =
            getByte ()
        member this.GetInt32 () =
            getConverted 4 System.BitConverter.ToInt32
        member this.GetUInt32 () =
            getConverted 4 System.BitConverter.ToUInt32


    type DexFile [<JavaScript>] private () =
        [<JavaScript>]
        member val strings = new ResizeArray<string>()
        [<JavaScript>]
        member val types = new ResizeArray<string>()
        [<JavaScript>]
        member val prototypes = new ResizeArray<string>()
        [<JavaScript>]
        member val fields = new ResizeArray<string>()
        [<JavaScript>]
        member val methods = new ResizeArray<string>()
        [<JavaScript>]
        member val classes = new ResizeArray<string>()

        [<JavaScript>]
        static member Init () =
            let a = new DexFile ()
            a
