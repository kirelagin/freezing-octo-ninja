namespace Dalvik

module VM =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<JavaScript>]
    let start (bytes : Uint8Array) =
        Dex.DexFileArray bytes