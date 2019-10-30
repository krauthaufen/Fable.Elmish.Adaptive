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

    type ReactUpdater(dst : Types.Element, ui : aval<ReactElement>) =
        inherit AdaptiveObject()

        member x.Update() = 
            x.EvaluateAlways AdaptiveToken.Top (fun t ->
                let ui = ui.GetValue t
                ReactDom.render(ui, dst)
            )

        override x.MarkObject() =
            setTimeout 0 x.Update
            false

    type EmptyOutputSet() =
        static let emptyArray : IAdaptiveObject[] = Array.zeroCreate 0
        interface IWeakOutputSet with
            member x.IsEmpty = true
            member x.Add _ = false
            member x.Remove _ = false
            member x.Consume() = emptyArray
    

    type CallbackObject(obj: IAdaptiveObject, callback: CallbackObject -> unit) =
        static let emptyOutputs = EmptyOutputSet() :> IWeakOutputSet
        let mutable level = obj.Level + 1
        let mutable live = 1
        let mutable obj = obj
        let mutable weak = null

        member x.Mark() =
            if live > 0 then callback x
            false

        member x.Dispose() =
            let l = live
            live <- 0
            if l = 1 then
                obj.Outputs.Remove x |> ignore
                obj <- Unchecked.defaultof<_>
                weak <- null
                level <- 0

        interface IDisposable with
            member x.Dispose() = x.Dispose()

        interface IAdaptiveObject with
            member x.Tag
                with get() = null
                and set _ = ()

            member x.Weak =
                let w = weak
                if isNull w then 
                    let w = WeakReference<IAdaptiveObject>(x)
                    weak <- w
                    w
                else
                    w
            member x.InputChanged(_,_) = ()
            member x.AllInputsProcessed(_) = ()
            member x.IsConstant = false
            member x.OutOfDate
                with get() = false
                and set _ = ()
            member x.Outputs = emptyOutputs
            member x.Mark() = x.Mark()
            member x.Level
                with get() = level
                and set l = level <- l
    
    type IAdaptiveObject with
        /// Registers a callback with the given object that will be executed
        /// whenever the object gets marked out-of-date.
        /// Note that it does not trigger when the object is currently out-of-date.
        /// Returns a disposable for removing the callback.
        member x.AddMarkingCallbackLocal (callback: unit -> unit) =
            let cb =
                new CallbackObject(x, fun self ->
                    try callback ()
                    finally x.Outputs.Add self |> ignore
                )

            x.Outputs.Add cb |> ignore
            cb :> IDisposable

    module Sepp =

        let useAdaptive (v: aval<'T>) =
            // initialize hook with initial value
            let stateHook = 
                Hooks.useStateLazy (fun () -> AVal.force v)

            let onChange () =
                Log.line "before change"
                // tell the hook that our value had changed
                stateHook.update (fun _ -> AVal.force v)
                Log.line "after change"

            let marking = 
                v.AddMarkingCallbackLocal onChange

            Hooks.useEffectDisposable (fun () -> marking)

            stateHook.current 


    let test () =
        //let test : {| a : int |} = {| a = 10 |}
        let myComp =
            FunctionComponent.Of (fun (v : {| value : aval<int> |}) ->    
                let vv = Sepp.useAdaptive v.value
                div [] [
                    str (string (vv))
                ]
            )

        let cnt = cval 5
        let click (e : Types.MouseEvent) =
            transact (fun () ->
                cnt.Value <- cnt.Value + 1
            )
            e.preventDefault()


        let mm = cnt |> AVal.map (fun v -> printfn "eval"; v)

        let ui = 
            div [ ] [
                ofFunction myComp {| value = mm |} []
                button [OnClick click] [ str "Yeah" ]
            ] |> AVal.constant
            //myComp cnt () |> AVal.constant
            //cnt |> AVal.map (fun cnt ->
            //    div [ Style [UserSelect UserSelectOptions.None]; OnClick click ] [
            //        str (sprintf "abc: %A" cnt)
            //    ]
            //)

       
       
        let dst = document.createElement("div")
        document.body.appendChild dst |> ignore

        let updater = ReactUpdater(dst, ui)
        updater.Update()




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



module SimpleApp =
    
    type Message =
        | Increment 
        | Decrement

    let init() =
        0
        
    let update (model : int) (message : Message) =
        match message with
        | Increment -> model + 1
        | Decrement -> model - 1

    let view (model : aval<int>) (emit : Message -> unit) =
        let headerAtttributes =
            att {
                model |> AVal.map (fun v ->
                    if v < 0 then style "color: red" |> Some
                    elif v > 0 then style "color: darkgreen" |> Some
                    else None
                )
            }
        div [] [
            h3 headerAtttributes "Simple Counter"
            str (AVal.map string model)
            br []
            button [click (fun () -> emit Increment)] "+"
            button [click (fun () -> emit Decrement)] "-"
        ]

    let app =
        {
            init = init
            update = update
            view = view
            unpersist = Unpersist.value
        }



[<EntryPoint>] 
let main argv =
    document.addEventListener("readystatechange", fun _ ->
        if document.readyState = "complete" then
            Benchmark.test()
            //let d = App.run document.body None SimpleApp.app
            ()
            //Benchmark.test()
            //document.body?style?display <- "flex"
            //document.body?style?flexWrap <- "wrap"

            //let changeSize = 200
            //for size in [1000;5000;10000] do
            //    Benchmark.runReact size 1 150
            //    Benchmark.runAdaptive size 1 150

            //for c in 1000 .. 1000 .. 5000 do
            //    //Benchmark.runReact c 50
            //    Benchmark.runAdaptive c 50 1
            //for c in 100 .. 100 .. 5000 do
            //    Benchmark.runAdaptive c

    )


    


    //document.write (sprintf "c: %A"  (AVal.force c))
    0 // return an integer exit code
