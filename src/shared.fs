namespace Dalvik

module Shared =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    type ResourceRequest =
        | GetObjectType of Dex.dref
        | RequestClass of Dex.Type
        | CreateInstance of Dex.Type
        | CreateArray of int32 * Dex.Type
        | FillArray of Dex.dref * Dex.RegValue array
        | GetArrayLength of Dex.dref
        | GetArrayItem of Dex.dref * int32
        | PutArrayItem of Dex.dref * int32 * Dex.RegValue
        | GetStaticField of Dex.Field
        | PutStaticField of Dex.Field * Dex.RegValue
        | GetInstanceField of Dex.dref * Dex.Field
        | PutInstanceField of Dex.dref * Dex.Field * Dex.RegValue

    [<JavaScript>]
    type ResourceReply =
        | ProvideType of Dex.Type
        | ProvideClass of Dex.Class
        | ProvideInstance of Dex.dref
        | ProvideValue of Dex.RegValue
        | RequestProcessed