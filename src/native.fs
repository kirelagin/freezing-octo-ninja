namespace Dalvik

open IntelliFactory.WebSharper

[<JavaScript>]
module Native =
    open System.Collections.Generic
    open Coretypes
    open Shared

    let registerNativeMethod (m, n) = Dalvik.ThreadWorker.nativelib.[m] <- n

    let genSignature (cls : string, name : string, ret : string, args : string array) : Dex.Method =
        let shorty = String.concat "" <| Array.map (fun (s : string) ->
                                                        let c = s.Substring (0, 1)
                                                        if c = "[" then "L" else c) (Array.append [| ret |] args)
        let proto = Dex.Proto (shorty, Dex.Type ret, Array.map Dex.Type args)
        Dex.Method (Dex.Type cls, proto, name)

    let init () =
        registerNativeMethod (genSignature ("Ljava/lang/System;", "currentTimeMillis",
                                            "J", [| |]),
            fun (args, cont) ->
                cont << Some << Store.storeLong << GLong.FromNumber <| (new Date ()).GetTime ()
        )
        registerNativeMethod (genSignature ("Llibcore/io/OsConstants;", "initConstants",
                                            "V", [| |]),
            fun (args, cont) ->
                cont None
        )
        registerNativeMethod (genSignature ("Ljava/io/PrintStream;", "println",
                                            "V", [| "I" |]),
            fun (args, cont) ->
                let v = Store.loadInt args.[1]
                ThreadWorker.requestInteraction (ConsoleLog v, fun () -> cont None)
        )
        registerNativeMethod (genSignature ("Ljava/io/PrintStream;", "println",
                                            "V", [| "J" |]),
            fun (args, cont) ->
                let v = (Store.loadLong args.[1]).ToString ()
                ThreadWorker.requestInteraction (ConsoleLog v, fun () -> cont None)
        )
        registerNativeMethod (genSignature ("Ljava/io/PrintStream;", "println",
                                            "V", [| "F" |]),
            fun (args, cont) ->
                let v = (Store.loadFloat args.[1]).ToString ()
                ThreadWorker.requestInteraction (ConsoleLog v, fun () -> cont None)
        )
        registerNativeMethod (genSignature ("Ljava/io/PrintStream;", "println",
                                            "V", [| "D" |]),
            fun (args, cont) ->
                let v = (Store.loadDouble args.[1]).ToString ()
                ThreadWorker.requestInteraction (ConsoleLog v, fun () -> cont None)
        )
        registerNativeMethod (genSignature ("Ljava/io/PrintStream;", "println",
                                            "V", [| "Ljava/lang/String;" |]),
            fun (args, cont) ->
                match args.[1] with
                | RegRef dref ->
                    ThreadWorker.instanceGet dref (Dex.Field (Dex.Type "Ljava/lang/String;", Dex.Type "[C", "value")) (fun value ->
                        match value with
                        | RegRef arref ->
                            ThreadWorker.getWholeArray arref (fun chars ->
                                let str = String.fromIntArray << Array.map Store.loadInt <| chars
                                ThreadWorker.requestInteraction (ConsoleLog str, fun () -> cont None)
                            )
                        | _ -> failwith "Wrong register type"
                    )
                | _ -> failwith "Wrong register type"
        )
