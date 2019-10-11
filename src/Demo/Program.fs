module Entry


open System
open Browser
open Fable.Core
open Fable.Core.JsInterop
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

[<Emit("performance.now()")>]
let now() : float = jsNative

module Benchmark =
    
    let timed (f : unit -> unit) =
        let t0 = now()
        f()
        let t1 = now()
        t1 - t0

    let appendCode header fmt =
        fmt |> Printf.kprintf (fun str ->
            let quote = document.createElement "blockquote"
            let pre = document.createElement "pre"
            let code = document.createElement "code"
            let h3 = document.createElement "h3"

            h3.innerHTML <- header

            quote.appendChild h3 |> ignore
            quote.appendChild pre |> ignore
            pre.appendChild code |> ignore
            code.innerText <- str

            document.body.appendChild quote |> ignore
        )

    let runAdaptive(cnt : int) =
        let list = IndexList.ofList [ 1 .. cnt ]
        let elements = clist list
        let root = document.createElement "div"
        root?style?display <- "none"
        document.body.appendChild root |> ignore

        let view =
            ul [] (
                elements |> AList.map (fun e ->
                    li [] (string e)
                )
            )

        let dummyScope =     
            { 
                Updater.subscribeAdaptive = fun _ _ _ -> 
                    { new IDisposable with member x.Dispose() = () } 
            }
        
        let updater = Updater.create root dummyScope view
        updater.Update(AdaptiveToken.Top)
        
        // warmup
        for i in 1 .. 20 do
            transact (fun () -> elements.Value <- IndexList.add i elements.Value)
            updater.Update AdaptiveToken.Top
            
        transact (fun () -> elements.Value <- list)
        updater.Update AdaptiveToken.Top


        let rand = System.Random()
        let indices = List.init 100 (fun _ -> list.TryGetIndex(rand.Next(elements.Count)).Value)

        let mutable updateTime = 0.0
        let mutable transactTime = 0.0
        let totalTime =
            timed (fun () ->
                for i in indices do
                    transactTime <- transactTime +
                        timed (fun () ->
                            transact (fun () -> elements.[i] <- 123)
                        )
                    updateTime <- updateTime +
                        timed (fun () -> updater.Update AdaptiveToken.Top)
            )

        updater.Node.remove()
        appendCode (string cnt) "transact: %.3fms\nupdate:   %.3fms\ntotal:    %.3fms" transactTime updateTime totalTime


    open Elmish
    open Elmish.React
    open Fable.React

    let runReact(cnt : int) =
        let init _ = []
        let update _ m = m
        let view _ _ = div [] [str "hey"]

        let div = document.createElement "div"
        div.id <- "elmish-app"
        document.body.appendChild div |> ignore

        Program.mkSimple init update view
        |> Program.withConsoleTrace
        |> Program.withReactBatched "elmish-app"
        |> Program.run


let demo() =
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



[<EntryPoint>] 
let main argv =
    document.addEventListener("readystatechange", fun _ ->
        if document.readyState = "complete" then
            document.body?style?display <- "flex"
            document.body?style?flexWrap <- "wrap"
            Benchmark.runReact 10
            //for c in 100 .. 100 .. 5000 do
            //    Benchmark.runAdaptive c

    )


    


    //document.write (sprintf "c: %A"  (AVal.force c))
    0 // return an integer exit code
