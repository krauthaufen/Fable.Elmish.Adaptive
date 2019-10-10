module Entry


open System
open Browser
open Fable.Core
open FSharp.Data.Adaptive
open Fable.Elmish.Adaptive
open Fable.Elmish.Adaptive.Generic

module Test =
    
    let test (elements : alist<string>) =
        div [ clazz "myclass" ] [
            div [] "Header"
            div [] []

            br []
            br []

            div (att { clazz "foo"; AVal.constant (clazz "someclass")}) (
                elements |> AList.map (fun e ->
                    div [clazz "element"; click id; mousemove id] e
                )
            )

        ]


[<Emit("setTimeout($1, $0)")>]
let setTimeout (delay : int) (f : unit -> unit) : unit = jsNative

[<EntryPoint>]
let main argv =
    document.addEventListener("readystatechange", fun _ ->
        if document.readyState = "complete" then

            let model = cval 1
            let list = clist [ 1 ]

            let next() =
                transact (fun () ->
                    let v = model.Value * 11
                    model.Value <- v
                    list.Add v |> ignore
                )

            let clear() =
                transact (fun () ->
                    model.Value <- 1
                    list.Value <- IndexList.ofList [1]
                )
                

            let view =
                div [] [
                    input (
                        att { 
                            "type", "button"
                            "value", model |> AVal.map string
                            click next
                        }
                    )
                    input (
                        att { 
                            "type", "button"
                            "value", "Clear"
                            click clear
                        }
                    )

                    ul [] (
                        list |> AList.map (fun e ->
                            console.warn e
                            li [] (string e)
                        )
                    )
                ]

            let dummyScope =     
                { 
                    Updater.subscribeAdaptive = fun _ _ _ -> 
                        { new IDisposable with member x.Dispose() = () } 
                }

            let updater = Updater.create document.body dummyScope view

            let pull =
                { new AdaptiveObject() with
                    override x.MarkObject() =
                        setTimeout 0 (fun () ->
                            x.EvaluateAlways AdaptiveToken.Top (fun t ->
                                updater.Update t
                            )
                        )
                        false
                }

            pull.EvaluateAlways AdaptiveToken.Top (fun t ->
                updater.Update t
            )



    )


    


    //document.write (sprintf "c: %A"  (AVal.force c))
    0 // return an integer exit code
