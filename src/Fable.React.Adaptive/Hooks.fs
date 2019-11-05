namespace Fable.React.Adaptive

open FSharp.Data.Adaptive
open Fable.React
open Fable.JsHelpers

[<AutoOpen>]
module AdaptiveExtensions =


    let mutable hugo = 0

    type IHooks with
        /// uses the given aval as react-state-hook
        member __.useAdaptive (value: aval<'T>) =
            let mutable stateHook : IStateHook<'T> = Unchecked.defaultof<_>

            let onChange () =   
                Log.line "change"
                // tell the hook that our value had changed
                Timeout.set 0 (fun () ->
                    stateHook.update (fun _ -> AVal.force value)
                ) |> ignore

            let marking() =       
                hugo <- hugo + 1
                Log.line "create cb %d" hugo
                // whenever the value may have changed tell react to update the value (lazily)
                let d = value.AddMarkingCallback onChange
                { new System.IDisposable with
                    member x.Dispose() =
                        hugo <- hugo - 1
                        Log.line "dispose cb: %d" hugo
                        d.Dispose()
                }
            
            // tell react about the disposable subscription made.
            // NOTE that react registers the new callback BEFORE
            //      releasing the old one. That means that the internal `MultiCallbackObject` from
            //      FSharp.Data.Adaptive will *survive* during updates, making the whole process 
            //      quite efficient.
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


