namespace Dalvik

module Numbers =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<Inline "$x >>> 0">]
    let unsign (x : int) : int = X<_>