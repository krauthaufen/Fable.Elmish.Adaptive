namespace FSharp.Data.Adaptive

open Fable.Core

module AVal =
    type private PromiseValue<'T1, 'T2>(p : JS.Promise<'T1>, map : 'T1 -> 'T2, def : 'T2) as this =
        inherit AdaptiveObject()
        let mutable value = def
        let mutable constant = false

        do  p.``then``(map >> this.GotValue) |> ignore

        member private x.GotValue(v : 'T2) : unit =
            transact (fun () -> 
                lock x (fun () ->
                    value <- v
                    constant <- true
                    x.MarkOutdated()
                )
            )

        member x.GetValue(t : AdaptiveToken) =
            x.EvaluateAlways t (fun t ->
                value  
            )

        interface IAdaptiveObject with
            member x.IsConstant = constant
            member x.Weak = x.Weak
            member x.Outputs = x.Outputs
            member x.Mark() = x.MarkObject()
            member x.AllInputsProcessed(t) = x.AllInputProcessedObject(t)
            member x.InputChanged(t, o) = x.InputChangedObject(t, o)
        
            member x.Tag
                with get() = x.Tag
                and set o = x.Tag <- o

            member x.OutOfDate
                with get() = x.OutOfDate
                and set o = x.OutOfDate <- o

            member x.Level
                with get() = x.Level
                and set l = x.Level <- l
        
        interface AdaptiveValue with
            member x.GetValueUntyped t = x.GetValue t :> obj
            member x.ContentType = typeof<obj>

        interface AdaptiveValue<'T2> with
            member x.GetValue t = x.GetValue t

    let ofPromise (p : JS.Promise<'T>) = 
        PromiseValue(p, Some, None) :> aval<_>

    let ofPromiseDefault (def : 'T) (p : JS.Promise<'T>) = 
        PromiseValue(p, id, def) :> aval<_>
        