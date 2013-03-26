namespace Dalvik

module VM =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<JavaScript>]
    let start (bytes : Uint8Array) =
        JavaScript.Alert(bytes.ToString())
