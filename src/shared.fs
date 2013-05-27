namespace Dalvik

open IntelliFactory.WebSharper
[<JavaScript>]
module Shared =
    open Coretypes

    type ResourceRequest =
        | GetObjectType of dref
        | RequestClass of Dex.Type
        | CreateInstance of Dex.Type
        | CreateArray of int32 * Dex.Type
        | FillArray of dref * RegValue array
        | GetWholeArray of dref
        | GetArrayLength of dref
        | GetArrayItem of dref * int32
        | PutArrayItem of dref * int32 * RegValue
        | GetStaticField of Dex.Field
        | PutStaticField of Dex.Field * RegValue
        | GetInstanceField of dref * Dex.Field
        | PutInstanceField of dref * Dex.Field * RegValue
        | EnterMonitor of dref
        | ExitMonitor of dref
    type ResourceReply =
        | ProvideType of Dex.Type
        | ProvideClass of Dex.Class
        | ProvideInstance of dref
        | ProvideValue of RegValue
        | ProvideArray of RegValue array
        | RequestProcessed

    type InteractionRequest =
        | ConsoleLog of obj
