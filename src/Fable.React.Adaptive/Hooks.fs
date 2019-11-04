namespace Fable.React.Adaptive

open FSharp.Data.Adaptive
open Fable.React

[<AutoOpen>]
module AdaptiveExtensions =
    type IHooks with
        /// uses the given aval as react-state-hook
        member __.useAdaptive (value: aval<'T>) =
            // initialize hook with initial value
            let stateHook = 
                // initially pass the value to the hook
                Hooks.useStateLazy (fun () -> AVal.force value)

            let onChange () =
                // tell the hook that our value had changed
                stateHook.update (fun _ -> AVal.force value)

            let marking =   
                // whenever the value may have changed tell react to update the value (lazily)
                value.AddMarkingCallback onChange

            // tell react about the disposable subscription made.
            // NOTE that react registers the new callback BEFORE
            //      releasing the old one. That means that the internal `MultiCallbackObject` from
            //      FSharp.Data.Adaptive will *survive* during updates, making the whole process 
            //      quite efficient.
            Hooks.useEffectDisposable (fun () -> marking)

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


