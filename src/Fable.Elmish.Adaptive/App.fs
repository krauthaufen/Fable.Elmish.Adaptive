namespace Fable.Elmish.Adaptive

open System
open Fable.JsHelpers
open FSharp.Data.Adaptive
open Fable.React

type Unpersist<'Model, 'AdaptiveModel> =
    {
        create  : 'Model -> 'AdaptiveModel
        update  : 'AdaptiveModel -> 'Model -> unit
    }
    
//module Unpersist =
    //[<GeneralizableValue>]
    //let value<'a> : Unpersist<'a, aval<'a>> =
    //    {
    //        create = fun v -> cval v :> aval<'a>
    //        update = fun t v -> (unbox<cval<'a>> t).Value <- v
    //    }
        
    //[<GeneralizableValue>]
    //let set<'a> : Unpersist<HashSet<'a>, aset<'a>> =
    //    {
    //        create = fun v -> cset v :> aset<'a>
    //        update = fun t v -> (unbox<cset<'a>> t).Value <- v
    //    }
        
    //[<GeneralizableValue>]
    //let list<'a> : Unpersist<IndexList<'a>, alist<'a>> =
    //    {
    //        create = fun v -> clist v :> alist<'a>
    //        update = fun t v -> (unbox<clist<'a>> t).Value <- v
    //    }

    //[<GeneralizableValue>]
    //let map<'k, 'v> : Unpersist<HashMap<'k, 'v>, amap<'k, 'v>> =
    //    {
    //        create = fun v -> cmap v :> amap<'k, 'v>
    //        update = fun t v -> (unbox<cmap<'k, 'v>> t).Value <- v
    //    }

type App<'Model, 'AdaptiveModel, 'Message> =
    {
        init        : unit -> 'Model
        update      : 'Model -> 'Message -> 'Model
        view        : 'AdaptiveModel -> ('Message -> unit) -> ReactElement
        unpersist   : Unpersist<'Model, 'AdaptiveModel>
    }

module App =
    [<AutoOpen>]
    module private Helpers = 
        open System.Collections.Generic

        let startThread (ms : int) (perform : seq<'T> -> unit) : 'T -> unit =
            if ms < 0 then
                Set.singleton >> perform
            else
                let mutable pending = List<'T>()
                let mutable timeout = null
                let mutable timeoutCreationTime = 0.0

                let iter() =
                    let delay = Performance.now() - timeoutCreationTime
                    Log.line "timeout was delayed for %.2fms (%d entries)" delay pending.Count
                    if not (isNull timeout) then
                        Timeout.clear timeout
                        timeout <- null

                    let mine = pending
                    pending <- List<'T>()
                    perform mine

                let emit (value : 'T) =
                    pending.Add value
                    if isNull timeout then
                        timeoutCreationTime <- Performance.now()
                        timeout <- Timeout.set ms iter

                emit

    open Fable.React.Props

    let run (parent : Browser.Types.Element) (batchTime : Option<int>) (app : App<'Model, 'AdaptiveModel, 'Message>) =
        let mutable model = app.init()
        let mutable startRender = Performance.now()
        let mutable start = startRender
        let mutable unpersistEnd = startRender
        let mutable commitEnd = startRender

        let amodel = app.unpersist.create model
        Fable.React.Adaptive.AdaptiveComponents.addCallback (fun time ->
            let now = Performance.now()
            let total = now - start
            let render = now - startRender
            let unpersist = unpersistEnd - start
            let transact = commitEnd - unpersistEnd
            let react = startRender - commitEnd

            let stats = Fable.React.Adaptive.AdaptiveComponents.getTimes()

            let dst = Browser.Dom.document.getElementById("performance-report")
            if unbox dst then   
                let fmt = sprintf "%.2fms"
                let bb = Style [BorderTop "1px solid black"; BorderLeft "none"; BorderRight "none"]
                let padding = Style [PaddingRight "20px"]
                let rep =
                    table [ Style [FontFamily "monospace"; BorderCollapse "collapse"] ] [
                        tbody [] [
                            tr [] [ td [padding] [ str "unpersist" ]; td [] [str (fmt unpersist) ]]
                            tr [] [ td [padding; bb] [ str "commit" ]; td [bb] [str (fmt transact) ]]
                            tr [] [ td [padding; bb] [ str "render" ]; td [bb] [str (fmt react) ]]
                            tr [] [ td [padding; bb] [ str "wait" ]; td [bb] [str (fmt render) ]]
                            tr [] [ td [padding; bb] [ str "total" ]; td [bb] [str (fmt total) ]]
                            //tr [] [     
                            //    td [ Style [VerticalAlign "top"]; bb ] [ str "times" ]
                            //    td [ bb ] [
                            //        table [Style [BorderCollapse "collapse"]] [
                            //            tbody [] [
                            //                let mutable i = 0
                            //                for (k,v) in Map.toSeq stats do
                            //                    let atts =
                            //                        if i <> 0 then [padding:> IHTMLProp; bb :> IHTMLProp]
                            //                        else [padding :> IHTMLProp ]
                            //                    tr [] [ td atts [ str k ]; td atts [str (fmt v) ]]
                            //                    i <- i + 1
                            //            ]
                            //        ]
                            //    ]
                            //]
                        ]
                    ]
                ReactDom.render(rep, dst)
        )
        let emit =
            let batchTime = defaultArg batchTime -1
            startThread batchTime (fun msgs ->
                model <- (model, msgs) ||> Seq.fold app.update
                start <- Performance.now()


                let t = new Transaction()
                Transaction.using t (fun () ->
                    app.unpersist.update amodel model
                    unpersistEnd <- Performance.now()
                )
                t.Commit()
                commitEnd <- Performance.now()
                t.Dispose()
                startRender <- Performance.now()
            )
            

        let view = app.view amodel emit
        start <- Performance.now()
        unpersistEnd <- start
        commitEnd <- start
        ReactDom.render(view, parent)
        startRender <- Performance.now()

        { new IDisposable with 
            member x.Dispose() =
                ReactDom.unmountComponentAtNode parent |> ignore
        }