namespace Dalvik

module Shared =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    type ResourceRequest =
        | RequestClass of Dex.Type
        | ResolveMethod of Dex.dref * Dex.Method
        | CreateInstance of Dex.Type
        | GetStaticField of Dex.Field
        | PutStaticField of Dex.Field * Dex.RegValue
        | GetInstanceField of Dex.dref * Dex.Field
        | PutInstanceField of Dex.dref * Dex.Field * Dex.RegValue

    [<JavaScript>]
    type ResourceReply =
        | ProvideClass of Dex.Class
        | ProvideMethod of Dex.Type * Dex.MethodImpl
        | ProvideInstance of Dex.dref
        | ProvideValue of Dex.RegValue
        | RequestProcessed