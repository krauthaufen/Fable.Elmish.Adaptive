namespace Fable.React.Adaptive

open Browser
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.JsHelpers

type internal ReactPseudoParent() =

    let e = document.createElement("div")
    let mutable child : Types.Node = null
    let mutable latestOperation : option<JS.Promise<unit>> = None

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

            // let getter = n?__proto__?__lookupGetter__("parentNode")
            // defineProperty n "realParentNode" (fun () ->
            //     getter?call(n)
            // )

            n

        // fake removeChild (removing the one and only child)
        e?removeChild <- fun (n : Types.Node) ->
            if n = child then child <- null
            n

    member x.Current = child

    member x.Element =
        match latestOperation with
        | Some op -> 
            op |> Promise.map (fun () -> child)
        | None ->
            Promise.create (fun s _ -> s child)

    member x.Render(n : ReactElement) =
        run (fun success error ->
            ReactDom.render(n, e, fun () ->
                success child
            )
        )

    member x.Unmount() =
        run (fun success error ->
            let old = child
            if isNull old then
                success None
            else
                let res = ReactDom.unmountComponentAtNode e
                if res then
                    child <- null
                    success (Some old)
                else
                    success None
        )

module internal ComponentHelpers =
    let inline setDisplayName<'comp, 'props, 'state when 'comp :> Component<'props, 'state>> (name : string) =
        let t = ReactElementType.ofComponent<'comp, 'props, 'state>
        t?displayName <- name
