namespace Dalvik

[<AutoOpen>]
module FsUtil =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    let curry f a b = f (a, b)

    [<JavaScript>]
    let uncurry f (a,b) = f a b

    module Arrows =
        [<JavaScript>]
        let secondOf2 f (a, b) = (a, f b)

        [<JavaScript>]
        let secondOf3 f (a, b, c) = (a, f b, c)

        [<JavaScript>]
        let thirdOf3 f (a, b, c) = (a, b, f c)

        [<JavaScript>]
        let secondOf7 f (a, b, c, d, e, g, h) = (a, f b, c, d, e, g, h) // =)