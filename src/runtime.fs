namespace Dalvik

open IntelliFactory.WebSharper

[<JavaScript>]
module Runtime =
    open Coretypes    

    let getMethodImpl (Dex.Class (_, _, _, _, impl, _)) (direct : bool) (meth : Dex.Method) =
        match impl with
        | None -> None
        | Some (Dex.ClassImpl (_, _, dir, virt)) ->
            let d = if direct then dir else virt
            match Dumbdict.tryGet d meth with
            | None -> None
            | Some (_, None) -> None
            | Some (_, Some impl) -> Some impl

    let rec javatypeOfType (Dex.Type descr as t) =
        match descr with
        | "Z" -> Dex.JavaPrimitiveType <| Dex.BooleanType
        | "B" -> Dex.JavaPrimitiveType <| Dex.ByteType
        | "S" -> Dex.JavaPrimitiveType <| Dex.ShortType
        | "C" -> Dex.JavaPrimitiveType <| Dex.CharType
        | "I" -> Dex.JavaPrimitiveType <| Dex.IntType
        | "J" -> Dex.JavaPrimitiveType <| Dex.LongType
        | "F" -> Dex.JavaPrimitiveType <| Dex.FloatType
        | "D" -> Dex.JavaPrimitiveType <| Dex.DoubleType
        | StartsWith "L" rest -> Dex.JavaReferenceType << Dex.ObjectType <| t
        | StartsWith "[" rest -> Dex.JavaReferenceType << Dex.ArrayType <| javatypeOfType (Dex.Type rest)
        | _ -> failwith <| "Invalid type descriptor: " + descr

    let defaultValue (Dex.Field (_, t, _)) =
        match javatypeOfType t with
        | Dex.JavaPrimitiveType Dex.LongType -> Store.storeLong <| GLong.FromInt 0
        | Dex.JavaPrimitiveType Dex.DoubleType -> Store.storeDouble 0.0
        | Dex.JavaPrimitiveType Dex.FloatType -> Store.storeFloat 0.0f
        | _ -> Store.storeInt 0