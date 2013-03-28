namespace Dalvik

module ByteCode =
    open IntelliFactory.WebSharper
    open IntelliFactory.WebSharper.Html5

    [<JavaScript>]
    type Instruction () =
        let a = ()