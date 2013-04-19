namespace Dalvik

module Shared =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    type ResourceRequest =
        | RequestClass of Dex.Type
        | ResolveMethod of Dex.dref * Dex.Method
        | CreateInstance of Dex.Type

    [<JavaScript>]
    type ResourceReply =
        | ProvideClass of Dex.Class
        | ProvideMethod of Dex.Type * Dex.MethodImpl
        | ProvideInstance of Dex.dref