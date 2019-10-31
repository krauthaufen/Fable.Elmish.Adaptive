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
            mutable nodes : IndexList<Types.HTMLElement * Types.Element>
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

    //open Fable.Core.JsInterop
    //type IReactDom with
    //    member x.renderToNewElement(e : ReactElement) =
    //        let t = e?``type``
    //        let props = e?props

            

    //        console.log(props)  
    //        let children : obj = e?props?children
    //        match children with
    //        | :? array<ReactElement> as c when c.Length = 1 ->
    //            let el = document.createElement(t)
    //            ReactDom.render(c.[0], el)
    //            el
    //        | :? string as s ->
    //            let el = document.createElement(t)
    //            el.innerText <- s
    //            el
                
    //        | _ ->
    //            let el = document.createElement("span")
    //            ReactDom.render(e, el)
    //            el

            
    [<Emit("arguments")>]
    let internal args : obj[] = jsNative


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
                    | Some (_, (dst,_)) ->
                        ReactDom.render(element, dst)
                    | None ->
                        match r with
                        | Some (_ri, (_rep, re)) ->
                            let dummy = document.createElement "div"
                            ReactDom.render(element, dummy, fun () ->
                                let actual = if dummy.children.length = 1 then dummy.children.[0] else dummy :> _
                                x.realstate.self.insertBefore(actual, re) |> ignore
                                x.realstate.nodes <- IndexList.set index (dummy, actual) x.realstate.nodes
                            )
                        | None ->
                            //let thing = ReactDom.renderToNewElement(element)
                            //x.realstate.self.appendChild(thing) |> ignore
                            //x.realstate.nodes <- IndexList.set index (thing, thing :> _) x.realstate.nodes

                            let dummy = document.createElement "div"
                            ReactDom.render(element, dummy, fun () ->
                                let actual = if dummy.children.length = 1 then dummy.children.[0] else dummy :> _
                                x.realstate.self.appendChild(actual) |> ignore
                                x.realstate.nodes <- IndexList.set index (dummy, actual) x.realstate.nodes
                            )

                | Remove ->
                    match IndexList.tryRemove index x.realstate.nodes with
                    | Some ((dummy, actual), rest) ->
                        x.realstate.nodes <- rest
                        actual.remove()
                        dummy.appendChild actual |> ignore
                        let worked = ReactDom.unmountComponentAtNode dummy 
                        if not worked then
                            console.warn("could not unmount", dummy)
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



module DebugComponents = 
    open Fable.React
    open Fable.Core.JsInterop
    open Fable.React.ReactiveComponents

    type LogComponent(value : State<ReactElement>) =
        inherit Component<State<ReactElement>, State<ReactElement>>(value)
        do base.setInitState { value = value.value }

        override x.componentWillUnmount() =
            console.log("unmount", x.state.value?props?children)

        override x.componentDidMount() =
            console.log("mount", x.state.value?props?children)

        override x.componentDidUpdate(_, _) =
            console.log("update", x.state.value?props?children)

        override x.render() =
            x.state.value
            
    let withLogging str = ofType<LogComponent, _, _> ({ value = str }) []

