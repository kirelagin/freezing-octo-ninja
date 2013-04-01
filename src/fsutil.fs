namespace Dalvik

[<AutoOpen>]
module FsUtil =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    let curry f a b = f (a, b)

    [<JavaScript>]
    let uncurry f (a,b) = f a b
