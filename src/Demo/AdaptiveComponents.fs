namespace React.Adaptive

open System
open Browser
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.ReactiveComponents
open Fable.React.Props
open FSharp.Data.Adaptive

module internal AdaptiveComponents =
    type ComponentState =
        {
            mutable subnodes : alist<ReactElement>
            mutable self : Types.Element 
            mutable reader : Option<IIndexListReader<ReactElement>>
            mutable sub : IDisposable
            mutable nodes : IndexList<Types.Element>
        }

        static member create (subnodes) =
            {
                subnodes = subnodes
                self = null
                reader = None
                sub = { new IDisposable with member x.Dispose() = () }
                nodes = IndexList.empty
            }
    type AListComponentState =
        {
            tag : string
            subnodes : alist<ReactElement>
        }

    type AListComponent(a : AListComponentState) =
        inherit Fable.React.Component<AListComponentState, State<ComponentState>>(a) 
        do base.setInitState({ value = ComponentState.create a.subnodes })

        member private x.realstate = x.state.value

        member private x.callback() =
            x.forceUpdate()

        member private x.getReader() =
            match x.realstate.reader with
            | Some r -> r
            | None -> 
                let r = x.realstate.subnodes.GetReader()
                x.realstate.sub <- r.AddMarkingCallback x.callback
                x.realstate.reader <- Some r
                r

        member private x.recreateReader() =
            let r = x.realstate.subnodes.GetReader()
            let s = r.AddMarkingCallback x.callback
            x.realstate.sub.Dispose()
            x.realstate.sub <- s
            x.realstate.reader <- Some r
            r

        member private x.update(r : IndexListDelta<ReactElement>) =
            for (index, op) in IndexListDelta.toSeq r do
                match op with
                | Set element ->
                    let (_l, s, r) = IndexList.neighbours index x.realstate.nodes
                    match s with
                    | Some (_, dst) ->
                        ReactDom.render(element, dst)
                    | None ->
                        match r with
                        | Some (_ri, re) ->
                            let d = document.createElement("span")
                            ReactDom.render(element, d, fun () ->
                                let d = if d.children.length = 1 then d.children.[0] else d :> _
                                x.realstate.self.insertBefore(d, re) |> ignore
                                x.realstate.nodes <- IndexList.set index d x.realstate.nodes
                            )
                        | None ->
                            let d = document.createElement("span")
                            ReactDom.render(element, d, fun () ->
                                let d = if d.children.length = 1 then d.children.[0] else d :> _
                                x.realstate.self.appendChild(d) |> ignore
                                x.realstate.nodes <- IndexList.set index d x.realstate.nodes
                            )

                | Remove ->
                    match IndexList.tryRemove index x.realstate.nodes with
                    | Some (node, rest) ->
                        x.realstate.nodes <- rest
                        node.remove()
                    | None ->
                        ()


        override x.componentDidMount() =
            let r = x.getReader()
            x.update(r.GetChanges AdaptiveToken.Top)

        override x.componentWillUnmount() =
            x.realstate.reader <- None

        override x.componentDidUpdate(newSubNodes : AListComponentState, state : State<ComponentState>) =
            if x.realstate.subnodes = newSubNodes.subnodes then
                let r = x.getReader()
                x.update (r.GetChanges AdaptiveToken.Top)
            else
                x.realstate.subnodes <- newSubNodes.subnodes
                match x.realstate.reader with
                | Some oldReader ->
                    let newReader = x.recreateReader()
                    newReader.GetChanges AdaptiveToken.Top |> ignore
                    let changes = IndexList.computeDelta oldReader.State newReader.State
                    x.update changes
                | None ->
                    let r = x.getReader()
                    x.update (r.GetChanges AdaptiveToken.Top)

        override x.render() =
            let props = [Ref (fun e -> x.realstate.self <- e)]
            ReactBindings.React.createElement(x.props.tag, keyValueList CaseRules.LowerFirst props, [])
          

        
    let adaptiveNode tag v = ofType<AListComponent, _, _> ({ tag = tag; subnodes = v }) []

[<AutoOpen>]
module AdaptiveTags =
    open AdaptiveComponents

    

    let adiv c = adaptiveNode "div" c
    let aol c = adaptiveNode "ol" c
    let aul c = adaptiveNode "ul" c
