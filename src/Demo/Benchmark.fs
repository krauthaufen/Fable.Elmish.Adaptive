module Benchmark
    
open System
open Browser
open Fable.Core
open Fable.Core.JsInterop
open FSharp.Data.Adaptive
open Fable.Elmish.Adaptive
open Fable.Elmish.Adaptive.Generic
open Fable.React.Adaptive


[<Emit("setTimeout($1, $0)")>]
let setTimeout (delay : int) (f : unit -> unit) : unit = jsNative

[<Emit("performance.now()")>]
let now() : float = jsNative

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

let runAdaptive(cnt : int) (changes : int) (changeSize : int) =
    let list = IndexList.ofList [ 1 .. cnt ]
    let elements = clist list
    let root = document.createElement "div"
    document.body.appendChild root |> ignore

    let view =
        ul [] (
            elements |> AList.map (fun e ->
                li [ clazz "yeah" ] [
                    div [ clazz "test" ] [
                        str (string e)
                    ]
                ]
            )
        )

    let dummyScope =     
        { 
            Updater.subscribeAdaptive = fun _ _ _ -> 
                { new IDisposable with member x.Dispose() = () } 
        }
        
    let mutable updater = Updater.create root dummyScope view
    updater.Update(AdaptiveToken.Top)

    // warmup
    for i in 1 .. 20 do
        transact (fun () -> elements.Value <- IndexList.add i elements.Value)
        updater.Update AdaptiveToken.Top
            
    updater.Destroy()
    transact (fun () -> elements.Value <- list)
    updater <- Updater.create root dummyScope view
        
    let initial = 
        timed (fun () ->
            updater.Update AdaptiveToken.Top
        )


    let rand = System.Random()
    let indices = 
        List.init changes (fun _ -> 
            List.init changeSize (fun _ ->
                list.TryGetIndex(rand.Next(elements.Count)).Value, rand.Next()
            )
        )

    let mutable updateTime = 0.0
    let mutable transactTime = 0.0
    let totalTime =
        timed (fun () ->
            for i in indices do
                transactTime <- transactTime +
                    timed (fun () ->
                        for (i,v) in i do
                            transact (fun () -> elements.[i] <- v)
                    )
                updateTime <- updateTime +
                    timed (fun () -> updater.Update AdaptiveToken.Top)
        )

    updater.Destroy()
    appendCode (sprintf "adaptive %d/%d" cnt changeSize) "initial: %.3fms\ntransact: %.3fms\nupdate:   %.3fms\ntotal:    %.3fms" initial (transactTime / float changes) (updateTime / float changes) (totalTime / float changes)




open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

let runReact(cnt : int) (changes : int) (changeSize : int) =
    let init _ = [1 .. cnt]
    let update _ m = m

        


    let view m _ = 
        ul [] (
            m |> List.map (fun e -> 
                li [classList ["yeah", true] ] [ 
                    div [classList ["test", true]] [
                        str (string e)
                    ] 
                ]
            )
        )

    let div = document.createElement "div"
    div.id <- "elmish-app"
    document.body.appendChild div |> ignore

    let a = 
        Program.mkSimple init update view
        |> Program.withReactSynchronous "elmish-app"
        
    a |> Program.run

       
    let setState (l : list<int>) = a?setState l

    let set (i : int) (value : int) (l : list<int>) =
        List.take i l @ [value] @ List.skip (i+1) l

    let rand = System.Random()
    let otherLists =
        List.init changes (fun _ ->
            let mutable l = init()
            for _ in 1 .. changeSize do
                let idx = rand.Next cnt
                l <- set idx (rand.Next()) l
            l
        )

    // warmup
    for i in Seq.truncate 10 otherLists do setState i
    setState []


    let data = init()

    let initial =
        timed (fun () -> setState data)


    let took = 
        timed (fun () ->
            for l in otherLists do
                setState l
        )

    appendCode (sprintf "react %d/%d" cnt changeSize) "initial: %.3fms\nupdate: %.3fms" initial (took / float changes)
    div.remove()
