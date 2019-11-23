namespace Fable.Elmish.Adaptive

open System
open Fable.React.Adaptive.JsHelpers
open FSharp.Data.Adaptive
open Fable.React
open Adaptify


type Unpersist<'Model, 'AdaptiveModel> = Adaptify.Unpersist<'Model, 'AdaptiveModel>

module Unpersist =
    
    let create (init : 'T -> 'AdaptiveT) (update : 'AdaptiveT -> 'T -> unit) = 
        Adaptify.Unpersist.create init update

    [<GeneralizableValue>]
    let aval<'T> = Adaptify.Unpersist.aval<'T>
        
    [<GeneralizableValue>]
    let aset<'T> = Adaptify.Unpersist.aset<'T>
        
    [<GeneralizableValue>]
    let alist<'T> = Adaptify.Unpersist.alist<'T>

    [<GeneralizableValue>]
    let amap<'K, 'V> = Adaptify.Unpersist.amap<'K, 'V>

    let inline instance< ^T, ^AdaptiveT when (^T or ^AdaptiveT) : (static member Unpersist : Unpersist< ^T, ^AdaptiveT >) > =
        ((^T or ^AdaptiveT) : (static member Unpersist : Unpersist< ^T, ^AdaptiveT >) ())

    


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
        let mutable flushEnd = Performance.now()
        let mutable start = Performance.now()

        let amodel = app.unpersist.init model

        // install performance-report if element exists.
        let dst = Browser.Dom.document.getElementById("adaptive-performance-report")
        if unbox dst then
            Fable.React.Adaptive.AdaptiveComponents.addCallback (fun time ->

                let now = Performance.now()
                let total = now - start
                Fable.React.Adaptive.AdaptiveComponents.addTime "wait" (now - flushEnd)
            
                let stats = Fable.React.Adaptive.AdaptiveComponents.getTimes()
                if unbox dst then   
                    let fmt = sprintf "%.2fms"
                    let bb = Style [BorderTop "1px solid black"; BorderLeft "none"; BorderRight "none"]
                    let padding = Style [PaddingRight "20px"]

                    let mutable sum = 0.0
                    let rep =
                        table [Style [FontFamily "monospace"; BorderCollapse "collapse"]] [
                            tbody [] [
                                let mutable i = 0
                                for (k,v) in Map.toSeq stats do
                                    let atts =
                                        if i <> 0 then [padding:> IHTMLProp; bb :> IHTMLProp]
                                        else [padding :> IHTMLProp ]
                                    tr [] [ td atts [ str k ]; td atts [str (fmt v) ]]
                                    i <- i + 1
                                    sum <- sum + v

                                tr [] [ td [padding:> IHTMLProp; bb :> IHTMLProp] [ str "unknown" ]; td [padding:> IHTMLProp; bb :> IHTMLProp] [str (fmt (total - sum)) ]]

                                tr [] [ td [padding:> IHTMLProp; bb :> IHTMLProp] [ str "total" ]; td [padding:> IHTMLProp; bb :> IHTMLProp] [str (fmt total) ]]
                            ]
                        ]
                         
                    ReactDom.render(rep, dst)
            )

        let emit =
            let batchTime = defaultArg batchTime -1
            startThread batchTime (fun msgs ->
                model <- (model, msgs) ||> Seq.fold app.update

                start <- Performance.now()
                let dd = Fable.React.Adaptive.AdaptiveComponents.startMeasure "unpersist"
                let t = new Transaction()
                Transaction.using t (fun () ->
                    app.unpersist.update amodel model
                )
                dd.Dispose()

                let dd = Fable.React.Adaptive.AdaptiveComponents.startMeasure "transact"
                t.Commit()
                dd.Dispose()
                
                let dd = Fable.React.Adaptive.AdaptiveComponents.startMeasure "dispose"
                t.Dispose()
                dd.Dispose()
                flushEnd <- Performance.now()

            )
            

        let view = app.view amodel emit
        start <- Performance.now()
        flushEnd <- start
        ReactDom.render(view, parent)

        { new IDisposable with 
            member x.Dispose() =
                ReactDom.unmountComponentAtNode parent |> ignore
        }