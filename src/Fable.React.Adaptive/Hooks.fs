namespace Fable.React.Adaptive

open FSharp.Data.Adaptive
open Fable.React
open Fable.React.Adaptive.JsHelpers

[<AutoOpen>]
module AdaptiveHookExtensions =
    type IHooks with
        /// uses the given aval as react-state-hook
        member __.useAdaptive (value: aval<'T>) =
            let mutable stateHook : IStateHook<'T> = Unchecked.defaultof<_>

            let onChange () =   
                // tell the hook that our value had changed
                Timeout.set 0 (fun () ->
                    stateHook.update (fun _ -> AVal.force value)
                ) |> ignore

            let marking() =       
                // whenever the value may have changed tell react to update the value (lazily)
                value.AddMarkingCallback onChange
            
            /// tell react about the disposable subscription made.
            /// NOTE that react registers the new callback AFTER
            ///      releasing the old one. That means that the internal `MultiCallbackObject` from
            ///      FSharp.Data.Adaptive will not *survive* during updates, making the whole process 
            ///      quite inefficient currently!
            Hooks.useEffectDisposable marking

            // initialize hook with initial value
            stateHook <-
                // initially pass the value to the hook
                Hooks.useStateLazy (fun () -> AVal.force value)

            stateHook.current 
            
        /// uses the given aset as react-state-hook
        member inline x.useAdaptive (value: aset<'T>) =
            x.useAdaptive value.Content
            
        /// uses the given amap as react-state-hook
        member inline x.useAdaptive (value: amap<'K, 'V>) =
            x.useAdaptive value.Content
            
        /// uses the given alist as react-state-hook
        member inline x.useAdaptive (value: alist<'T>) =
            x.useAdaptive value.Content

        /// uses the given cset as react-state-hook
        member inline x.useAdaptive (value: cset<'T>) =
            x.useAdaptive (value :> aset<'T>).Content

        /// uses the given cmap as react-state-hook
        member inline x.useAdaptive (value: cmap<'K, 'V>) =
            x.useAdaptive (value :> amap<'K, 'V>).Content

        /// uses the given clist as react-state-hook
        member inline x.useAdaptive (value: clist<'T>) =
            x.useAdaptive (value :> alist<'T>).Content

