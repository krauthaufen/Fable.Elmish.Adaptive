namespace Fable.Elmish.Adaptive

open FSharp.Data.Adaptive

module Log =
    open Browser

    let inline line fmt =
        Printf.kprintf (fun str -> console.log(str)) fmt
    let inline warn fmt =
        Printf.kprintf (fun str -> console.warn(str)) fmt
    let inline error fmt =
        Printf.kprintf (fun str -> console.error(str)) fmt




type Unpersist<'Model, 'AdaptiveModel> =
    {
        create  : 'Model -> 'AdaptiveModel
        update  : 'AdaptiveModel -> 'Model -> unit
    }
    
module Unpersist =
    [<GeneralizableValue>]
    let value<'a> : Unpersist<'a, aval<'a>> =
        {
            create = fun v -> cval v :> aval<'a>
            update = fun t v -> (unbox<cval<'a>> t).Value <- v
        }
        
    [<GeneralizableValue>]
    let set<'a> : Unpersist<HashSet<'a>, aset<'a>> =
        {
            create = fun v -> cset v :> aset<'a>
            update = fun t v -> (unbox<cset<'a>> t).Value <- v
        }
        
    [<GeneralizableValue>]
    let list<'a> : Unpersist<IndexList<'a>, alist<'a>> =
        {
            create = fun v -> clist v :> alist<'a>
            update = fun t v -> (unbox<clist<'a>> t).Value <- v
        }

type App<'Model, 'AdaptiveModel, 'Message> =
    {
        init        : unit -> 'Model
        update      : 'Model -> 'Message -> 'Model
        view        : 'AdaptiveModel -> ('Message -> unit) -> DomNode
        unpersist   : Unpersist<'Model, 'AdaptiveModel>
    }

module App =
    [<AutoOpen>]
    module private Helpers = 
        open Fable.Core
        open Browser

        [<AllowNullLiteral>]
        type ITimeout = interface end

        [<Emit("setTimeout($1, $0)")>]
        let setTimeout (delay : int) (f : unit -> unit) : ITimeout = jsNative

        [<Emit("clearTimeout($0)")>]
        let clearTimeout (t : ITimeout) : unit = jsNative

        let startThread (ms : int) (perform : seq<'T> -> unit) : 'T -> unit =
            let mutable pending = System.Collections.Generic.List<'T>()
            let mutable timeout = null
            let rec iter() =
                if not (isNull timeout) then
                    clearTimeout timeout
                    timeout <- null

                let mine = pending
                pending <- System.Collections.Generic.List<'T>()
                perform mine

            let emit (value : 'T) =
                pending.Add value
                if isNull timeout then
                    timeout <- setTimeout ms iter

            emit

        type RunningApp(u : Updater.NodeUpdater) =
            inherit AdaptiveObject()

            let mutable timeout = null
            let mutable isDisposed = false

            override x.MarkObject() =
                if not (isNull timeout) then 
                    clearTimeout timeout
                timeout <- setTimeout 0 (fun () ->
                    x.Update()
                )
                false

            member x.Update() =
                timeout <- null
                if not isDisposed then
                    x.EvaluateAlways AdaptiveToken.Top (fun t ->
                        u.Update(t)
                    )

            member x.Dispose() =
                isDisposed <- true

                if not (isNull timeout) then
                    clearTimeout timeout
                    timeout <- null

                u.Outputs.Remove x |> ignore
                u.Destroy()

            interface System.IDisposable with
                member x.Dispose() = x.Dispose()

    let run (parent : Browser.Types.HTMLElement) (batchTime : Option<int>) (app : App<'Model, 'AdaptiveModel, 'Message>) =
        let mutable model = app.init()
        let amodel = app.unpersist.create model

        let emit =
            match batchTime with
            | Some batchTime when batchTime >= 0 ->
                startThread batchTime (fun msgs ->
                    model <- (model, msgs) ||> Seq.fold app.update
                    transact (fun () ->
                        app.unpersist.update amodel model
                    )
                )
            | _ ->
                fun msg ->
                    model <- app.update model msg
                    transact (fun () ->
                        app.unpersist.update amodel model
                    )


        let view = app.view amodel emit

        let scope = { Updater.Scope.subscribeAdaptive = fun _ _ _ -> failwith "" }
        let updater = Updater.NodeUpdater.Create(parent, scope, view)

        let app = new RunningApp(updater) 
        app.Update()
        app :> System.IDisposable

