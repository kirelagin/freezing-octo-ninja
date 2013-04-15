namespace Dalvik

module Shared =
    open IntelliFactory.WebSharper

    type dref = int

    [<JavaScript>]
    type ResourceRequest =
        | RequestType of uint16
        | RequestClass of Dex.Type
        | RequestMethod of uint16
        | ResolveMethod of dref * uint16
        | CreateInstance of uint16

    [<JavaScript>]
    type ResourceReply =
        | ProvideType of Dex.Type
        | ProvideClass of Dex.Class
        | ProvideMethod of Dex.Method
        | ProvideInstance of dref