namespace Dalvik

[<AutoOpen>]
module FsUtil =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    let curry f a b = f (a, b)

    [<JavaScript>]
    let uncurry f (a,b) = f a b

    [<AutoOpen>]
    [<JavaScript>]
    module String =
        let (|StartsWith|_|) (p : string) (s : string) =
            if s.StartsWith p then
                Some <| s.Substring p.Length
            else
                None

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

    [<JavaScript>]
    module Dumbdict =
        type dumbdict<'k, 'v> = ('k * 'v) array

        let empty () : dumbdict<'k, 'v> =
            [| |]
        let addNoRepeat (d : dumbdict<'k, 'v>) (k : 'k) (v : 'v) =
            Array.push d (k, v)
        let tryGetWith (eq : 'k -> 'k -> bool) (d : dumbdict<'k, 'v>) (k : 'k) =
            match Array.tryFind (fun (k1, _) -> eq k k1) d with
            | Some (_, v) -> Some v
            | None -> None
        let tryGet (d : dumbdict<'k, 'v>) (k : 'k) = tryGetWith (=) d k
