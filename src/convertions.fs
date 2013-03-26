namespace Dalvik

[<AutoOpen>]
module Conversions =
    open IntelliFactory.WebSharper

    [<Inline "$0">]
    let inline uint16 x = uint16 x

    [<Inline "$0">]
    let inline int32 x = int32 x

    // Don't forget to call this when working with bit-operations
    // even if you think you already got an unsigned int!
    [<Inline "$0 >>> 0">]
    let inline uint32 x = uint32 x

    [<Inline "$0">]
    let inline uint64 x = uint64 x