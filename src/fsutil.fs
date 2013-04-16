namespace Dalvik

[<AutoOpen>]
module FsUtil =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    let curry f a b = f (a, b)

    [<JavaScript>]
    let uncurry f (a,b) = f a b

    [<JavaScript>]
    module Arrows =
        let secondOf2 f (a, b) = (a, f b)

        let secondOf3 f (a, b, c) = (a, f b, c)

        let thirdOf3 f (a, b, c) = (a, b, f c)

        let secondOf7 f (a, b, c, d, e, g, h) = (a, f b, c, d, e, g, h) // =)

    [<JavaScript>]
    module Dictionary =
        let tryGet (d : System.Collections.Generic.Dictionary<'k, 'v>) (k : 'k) =
            if d.ContainsKey k then Some <| d.[k] else None