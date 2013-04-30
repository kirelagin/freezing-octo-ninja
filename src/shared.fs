namespace Dalvik

module Shared =
    open IntelliFactory.WebSharper
    open Coretypes

    [<JavaScript>]
    type ResourceRequest =
        | GetObjectType of dref
        | RequestClass of Dex.Type
        | CreateInstance of Dex.Type
        | CreateArray of int32 * Dex.Type
        | FillArray of dref * RegValue array
        | GetArrayLength of dref
        | GetArrayItem of dref * int32
        | PutArrayItem of dref * int32 * RegValue
        | GetStaticField of Dex.Field
        | PutStaticField of Dex.Field * RegValue
        | GetInstanceField of dref * Dex.Field
        | PutInstanceField of dref * Dex.Field * RegValue

    [<JavaScript>]
    type ResourceReply =
        | ProvideType of Dex.Type
        | ProvideClass of Dex.Class
        | ProvideInstance of dref
        | ProvideValue of RegValue
        | RequestProcessed