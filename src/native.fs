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
        registerNativeMethod (genSignature ("LJslog;", "log",
                                            "V", [| "I" |]),
            fun (args, cont) ->
                let v = Store.loadInt args.[0]
                ThreadWorker.requestResource (ConsoleLog v, fun r ->
                    match r with
                    | RequestProcessed -> cont None
                    | _ -> failwith "Unexpected reply")
        )
        registerNativeMethod (genSignature ("LJslog;", "log",
                                            "V", [| "J" |]),
            fun (args, cont) ->
                let v = (Store.loadLong args.[0]).ToString ()
                ThreadWorker.requestResource (ConsoleLog v, fun r ->
                    match r with
                    | RequestProcessed -> cont None
                    | _ -> failwith "Unexpected reply")
        )
