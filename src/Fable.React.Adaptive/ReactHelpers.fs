namespace Fable.React.Adaptive

open Browser
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Adaptive.JsHelpers

/// Internal tool for *faking* parent nodes for React. 
/// 
/// **The Problem**
/// 
/// `ReactDom.render` expects a parent node to render to, that
/// will only hold one child managed by React internally.   
/// In our `AdaptiveComponent` use-case this is somewhat impractical, since our
/// `alist` already provides us with the needed information where to update what and
/// wrapping nodes in *dummy* divs is not really an option (e.g. ul needs to contain li elements directly)
/// 
/// **Current solution**
/// 
/// Therefore we create a real HTML <div> that is never actually part of the DOM, 
/// override its `firstChild`, `appendChild`, `removeChild` by locally defining them 
/// in order to make react beleive that the DOM it rendered is not modified, while
/// we actually move the created element somewhere else in the DOM.
type internal ReactPseudoParent private() =

    let e = document.createElement("div")
    let mutable child : Types.Node = null
    let mutable latestOperation : option<JS.Promise<unit>> = None

    /// Utility ensuring that (delayed) changes happen sequentially on the node.
    let run (render : ('a -> unit) -> (exn -> unit) -> unit) =
        match latestOperation with
        | Some r ->
            let res = r |> Promise.bind (fun () -> Promise.create render)
            latestOperation <- Some (unbox res)
            res
        | None ->
            let res = Promise.create render
            latestOperation <- Some (unbox res)
            res

    do  
        // fake firstChild (used by react interally)
        e.DefineProperty("firstChild", fun () -> child)

        // fake appendChild (setting our one and only child)
        e?appendChild <- fun (n : Types.Node) ->
            child <- n
            n.DefineProperty("parentNode", fun () -> e)
            n

        // fake removeChild (removing the one and only child)
        e?removeChild <- fun (n : Types.Node) ->
            if n = child then child <- null
            n

    /// The current Node contained in the ´ReactPseudoParent`.
    member x.Current = child

    /// The current Node contained in the ´ReactPseudoParent` as soon as all pending ops are finished.
    member x.Element =
        match latestOperation with
        | Some op -> 
            op |> Promise.map (fun () -> child)
        | None ->
            Promise.create (fun s _ -> s child)

    /// Renders the given ReactElement to the pseudo-parent and returns the (possibly new) resulting inner node.    
    member x.Render(n : ReactElement) =
        run (fun success error ->
            ReactDom.render(n, e, fun () ->
                success child
            )
        )

    /// Unmounts the component and returns the (optional) last Node contained in the fake-root.
    member x.Unmount() =
        run (fun success error ->
            let old = child
            let res = ReactDom.unmountComponentAtNode e
            if isNull old then
                success None
            else
                if res then
                    child <- null
                    success (Some old)
                else
                    success None
        )


    static member Create() =
        AdaptiveComponents.measure "pseudo" (fun () -> ReactPseudoParent())

module internal ComponentHelpers =
    /// Sets the display name for a custom component.
    let inline setDisplayName<'comp, 'props, 'state when 'comp :> Component<'props, 'state>> (name : string) =
        let t = ReactElementType.ofComponent<'comp, 'props, 'state>
        t?displayName <- name
