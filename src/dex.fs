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

        member this.Length with get () = data.Length
        member this.Seek (newOffset : uint64) =
            offset <- newOffset
        member this.GetInt8 () : int =
            int (getByte ())
        member this.GetInt16 () : int =
            let d = getBytes 2
            (int d.[1] <<< 8*1) ||| (int d.[0])
        member this.GetInt32 () : int32 =
            let d = getBytes 4
            (int d.[3] <<< 8*3) ||| (int d.[2] <<< 8*2) ||| (int d.[1] <<< 8*1) ||| (int d.[0])

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
