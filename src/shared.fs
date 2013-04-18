namespace Dalvik

module Shared =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    type ResourceRequest =
        | RequestClass of Dex.Type
        | ResolveMethod of Dex.dref * string * Dex.Proto
        | CreateInstance of Dex.Type

    [<JavaScript>]
    type ResourceReply =
        | ProvideClass of Dex.Class
        | ProvideMethod of Dex.Method
        | ProvideInstance of Dex.dref