namespace Dalvik

open IntelliFactory.WebSharper

[<JavaScript>]
module Runtime =
    open Dex

    let getMethodImpl (Class (_, _, _, _, impl)) (direct : bool) (meth : Dex.Method) =
        match impl with
        | None -> None
        | Some (ClassImpl (_, _, dir, virt)) ->
            let d = if direct then dir else virt
            match Dumbdict.tryGet d meth with
            | None -> None
            | Some (_, None) -> None
            | Some (_, Some impl) -> Some impl