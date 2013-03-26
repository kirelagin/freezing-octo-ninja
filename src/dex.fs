namespace Dalvik

module Dex =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    //type DexFileArray [<JavaScript>] (data : Uint8Array) =
    //    let offset = 0

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
