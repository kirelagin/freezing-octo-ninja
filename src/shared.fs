namespace Dalvik

module Shared =
    open IntelliFactory.WebSharper

    [<JavaScript>]
    type ResourceRequest =
        | RequestType of uint16
        | RequestClass of Dex.Type

    [<JavaScript>]
    type ResourceReply =
        | ProvideType of Dex.Type
        | ProvideClass of Dex.Class