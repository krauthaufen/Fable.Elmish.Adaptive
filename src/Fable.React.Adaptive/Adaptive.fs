namespace Fable.React.Adaptive

open System
open Browser
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.ReactiveComponents
open Fable.React.Props
open FSharp.Data.Adaptive
open Fable.React.Adaptive.JsHelpers
open CollectionsJS

type internal AdaptiveComponentProps =
    {   
        tag         : string
        attributes  : AttributeMap
        children    : alist<ReactElement>
    }

[<RequireQualifiedAccess>]
type internal ChildOperation =
    | Add of Index * ReactPseudoParent * Types.Node
    | Update of Types.Node * Types.Node
    | Remove of Index * option<Types.Node>

type internal AdaptiveComponentState(attributes : AttributeMap, children : alist<ReactElement>) =
    static let noDisposable = { new IDisposable with member x.Dispose() = () }

    let mutable children = children
    let mutable element : Types.Element = null
    let mutable parent : Types.Element = null
    let mutable reader : Option<IIndexListReader<ReactElement>> = None
    let mutable marking = noDisposable
    let mutable livingThigs : SortedMap<Index, ReactPseudoParent> = SortedMap.create()

    let mutable attributes = attributes
    let mutable attMarking = noDisposable
    let mutable att : Option<AttributeUpdater> = None

    let getReader(callback : unit -> unit) =
        match reader with
        | Some r -> r
        | None -> 
            let r = children.GetReader()
            marking <- r.AddMarkingCallback callback
            reader <- Some r
            r

    let getAttributeUpdater(callback : unit -> unit) =
        match att with
        | Some att -> att
        | None ->
            let u = AttributeUpdater(element, attributes)
            attMarking <- u.AddMarkingCallback callback
            att <- Some u
            u

    let recreateReader(callback : unit -> unit) =
        let r = children.GetReader()
        let s = r.AddMarkingCallback callback
        marking.Dispose()
        marking <- s
        reader <- Some r
        r
        
    let update (deltas : IndexListDelta<ReactElement>) =
        
        if deltas.Count > 0 then
            AdaptiveComponents.startRender()

            let rendering = AdaptiveComponents.startMeasure "ReactDom.render"

            let all = 
                IndexListDelta.toList deltas 
                |> List.choose (fun (index, op) ->
                    let s = SortedMap.tryFind index livingThigs
                    match op with
                    | Set react ->
                        match s with
                        | Some s -> 
                            let old = s.Current
                            let mapping (n : Types.Node) =
                                if n <> old then Some (ChildOperation.Update(old, n))
                                else None

                            s.Render(react) 
                            |> Promise.map mapping
                            |> Some
                        | None ->
                            let parent = ReactPseudoParent.Create()
                            parent.Render(react) 
                            |> Promise.map (fun n -> Some (ChildOperation.Add(index, parent, n)))
                            |> Some
                    | Remove ->
                        match s with
                        | Some s ->
                            s.Unmount()
                            |> Promise.map (fun n -> Some (ChildOperation.Remove(index, n)))
                            |> Some
                        | None ->
                            None
                )
                
            all |> Promise.all |> Promise.map (fun all ->
                rendering.Dispose()
                let mutate = AdaptiveComponents.startMeasure "mutate"
                for op in all do
                    match op with
                    | Some (ChildOperation.Update (o, n)) ->
                        element.replaceChild(n, o)  |> ignore
                    | Some (ChildOperation.Add(index, p, n)) ->
                        livingThigs |> SortedMap.alterNeigbours index (fun right ->
                            match right with
                            | Some r ->
                                element.insertBefore(n, r.Current) |> ignore
                            | None ->
                                element.appendChild(n) |> ignore
                            Some p
                        )
                    | Some (ChildOperation.Remove(index, o)) ->  
                        match o with
                        | Some o -> 
                            element.removeChild(o) |> ignore
                        | None -> ()
                        SortedMap.tryRemove index livingThigs |> ignore
                    | None ->
                        ()

                mutate.Dispose()
                AdaptiveComponents.stopRender()
                AdaptiveComponents.stopRender()
            )
        else
            AdaptiveComponents.stopRender()
            Promise.lift ()
                //for (index, op) in IndexListDelta.toSeq deltas do
                //    match op with
                //    | Set react ->  
                //        do!
                //            let t = AdaptiveComponents.startMeasure "alterNeigbours"
                //            livingThigs |> SortedMap.alterNeigboursPromise index (fun _l self right ->
                //                prom {
                //                    t.Dispose()
                //                    match self with
                //                    | Some dst ->  
                //                        let old = dst.Current

                //                        let d = AdaptiveComponents.startMeasure "render"
                //                        let! n = dst.Render(react)
                //                        d.Dispose()
                //                        if old <> n then 
                //                            AdaptiveComponents.measure "mutate" (fun () -> element.replaceChild(n, old) |> ignore)

                //                        return self
                //                    | None ->
                //                        match right with
                //                        | Some re ->
                //                            let parent = ReactPseudoParent.Create()
                //                            let d = AdaptiveComponents.startMeasure "render"
                //                            let! node = parent.Render(react)
                //                            d.Dispose()
                //                            let re = re.Current
                //                            AdaptiveComponents.measure "mutate" (fun () -> 
                //                                element.insertBefore(node, re) |> ignore
                //                            )
                //                            return Some parent

                //                        | None ->
                //                            let parent = ReactPseudoParent.Create()
                                
                //                            let d = AdaptiveComponents.startMeasure "render"
                //                            let! node = parent.Render(react)
                //                            d.Dispose()
                //                            AdaptiveComponents.measure "mutate" (fun () -> 
                //                                element.appendChild node |> ignore
                //                            )
                //                            return Some parent
                //                }
                //            )
                //    | Remove ->
                //        match SortedMap.tryRemove index livingThigs with
                //        | Some root ->
                //            let d = AdaptiveComponents.startMeasure "unmount"
                //            match! root.Unmount() with
                //            | Some e -> 
                //                d.Dispose()
                //                AdaptiveComponents.measure "mutate" (fun () -> element.removeChild e |> ignore)
                //            | None -> ()
                //        | None ->
                //            ()
            

    member x.Children = children

    member x.ReplaceElement (e : Types.Element) =
        if isNull e then 
            ()
        elif isNull element || element = e then
            element <- e
            parent <- e.parentElement
            match att with
            | Some att -> att.SetNode e
            | None -> ()
        else
            failwith "not implemented"
            //prom {
            //    for r in cache do
            //        let! c = r.Element
            //        e.appendChild c |> ignore
            //        ()
            //} |> ignore

            element <- e
            parent <- e.parentElement
            match att with
            | Some att -> att.SetNode e
            | None -> ()

    member x.ReplaceChildren (newList : alist<ReactElement>, callback : unit -> unit) =
        if children = newList then
            let r = getReader callback
            update (AdaptiveComponents.measure "eval" (fun () -> r.GetChanges AdaptiveToken.Top)) |> ignore
        else
            children <- newList
            match reader with
            | Some oldReader ->
                let newReader = recreateReader callback
                let changes =
                    AdaptiveComponents.measure "eval" (fun () -> 
                        newReader.GetChanges AdaptiveToken.Top |> ignore
                        IndexList.computeDelta oldReader.State newReader.State
                    )
                update changes |> ignore
            | None  ->
                let r = getReader callback
                update (AdaptiveComponents.measure "eval" (fun () -> r.GetChanges AdaptiveToken.Top)) |> ignore

    member x.ReplaceAttributes (newAttributes : AttributeMap, callback : unit -> unit) =
        let up = getAttributeUpdater callback
        if attributes <> newAttributes then
            attributes <- newAttributes
            up.SetAttributes newAttributes
        up.Update AdaptiveToken.Top

    member x.Boot(callback : unit -> unit) =
        let u = getAttributeUpdater callback
        let r = getReader callback
        u.Update AdaptiveToken.Top
        update (AdaptiveComponents.measure "eval" (fun () -> r.GetChanges AdaptiveToken.Top)) |> ignore

    member x.Dispose() =
        prom {
            //failwith "not imp"
            //for e in cache do 
            //    match! e.Unmount() with
            //    | Some e -> element.removeChild e |> ignore 
            //    | None -> ()

            match att with
            | Some a -> a.Destroy()
            | None -> ()

            marking.Dispose()
            attMarking.Dispose()
            children <- AList.empty
            element <- null
            reader <- None
            marking <- noDisposable
            //cache <- IndexList.empty
            attributes <- AttributeMap.empty
            attMarking <- noDisposable
            att <- None
        } |> ignore

type internal AdaptiveComponent(a : AdaptiveComponentProps)  =
    inherit Fable.React.Component<AdaptiveComponentProps, State<AdaptiveComponentState>>(a) 
    static do ComponentHelpers.setDisplayName<AdaptiveComponent,_,_> "AdaptiveComponent"
    do base.setInitState({ value = AdaptiveComponentState(a.attributes, a.children) })
       
    member x.invalidate() = 
        AdaptiveComponents.measure "forceUpdate" (fun () ->
            AdaptiveComponents.startRender()
            x.forceUpdate()
        )

    member x.state = 
        base.state.value

    override x.componentDidMount() =
        AdaptiveComponents.startRender()
        x.state.Boot x.invalidate |> ignore

    override x.componentWillUnmount() =
        x.state.Dispose()

    override x.componentDidUpdate(_, _) =
        match Transaction.Running with
        | Some t ->
            t.AddFinalizer (fun () ->
                x.state.ReplaceAttributes(x.props.attributes, x.invalidate)
                x.state.ReplaceChildren(x.props.children, x.invalidate) |> ignore
            ) 
        | None ->
            x.state.ReplaceAttributes(x.props.attributes, x.invalidate)
            x.state.ReplaceChildren(x.props.children, x.invalidate) |> ignore

    override x.shouldComponentUpdate(_,_) =
        true

    override x.render() =
        ReactBindings.React.createElement(
            x.props.tag, 
            keyValueList CaseRules.LowerFirst [Ref x.state.ReplaceElement], 
            []
        )


type internal AdaptiveStringProps =
    {
        text : aval<string>
    }

type internal AdaptiveStringState() =
    static let noDisposable = { new IDisposable with member x.Dispose() = () }

    let mutable value = AVal.constant ""
    let mutable callback = fun () -> ()
    let mutable sub = noDisposable
    let mutable text = ""

    member x.Text = text 

    member x.Update(newValue : aval<string>, newCallback : unit -> unit) =
        if value <> newValue then
            sub.Dispose()
            value <- newValue
            callback <- newCallback
            sub <- value.AddMarkingCallback callback

        text <- AVal.force newValue

    member x.Dispose() =
        sub.Dispose()
        value <- AVal.constant ""
        text <- ""
        callback <- fun () -> ()

type internal AdaptiveStringComponent(a : AdaptiveStringProps)  =
    inherit Fable.React.Component<AdaptiveStringProps, State<AdaptiveStringState>>(a) 
    static do ComponentHelpers.setDisplayName<AdaptiveComponent,_,_> "AList"
    do base.setInitState({ value = AdaptiveStringState() })

    member x.state = 
        base.state.value

    member x.invalidate() = 
        match Transaction.Running with
        | Some t ->
            t.AddFinalizer (fun () ->
                x.state.Update(x.props.text, x.invalidate)
                x.forceUpdate()
            )
        | None -> 
            x.state.Update(x.props.text, x.invalidate)
            x.forceUpdate()
        

    override x.componentWillUnmount() =
        x.state.Dispose()

    override x.componentDidUpdate(_, _) = 
        x.state.Update(x.props.text, x.invalidate)

    override x.render() =
        x.state.Update(x.props.text, x.invalidate)
        str x.state.Text 

module AdaptiveComponent =

    let string (text : aval<string>) =
        if text.IsConstant then
            str (AVal.force text)
        else
            let typ = Fable.React.ReactElementType.ofComponent<AdaptiveStringComponent,_,_>
            Fable.React.ReactElementType.create typ { text = text } []
        

    let create (tag : string) (attributes : AttributeMap) (children : alist<ReactElement>) =
        if children.IsConstant && attributes.IsConstant then
            let children =
                children.Content
                |> AVal.force
                |> IndexList.toList

            let props =
                attributes
                |> AttributeMap.force
                |> HashMap.toList
                |> createObj

            let typ = Fable.React.ReactElementType.ofHtmlElement tag
            Fable.React.ReactElementType.create typ props children
        //elif children.IsConstant then
        //    let children =
        //        children.Content
        //        |> AVal.force
        //        |> IndexList.toList
                
        //    let typ = Fable.React.ReactElementType.ofComponent<AdaptiveAttributesComponent,_,_>
        //    Fable.React.ReactElementType.create typ { tag = tag; attributes = attributes; children = children } []
        //elif attributes.IsConstant then
        //    let props =
        //        attributes.Content
        //        |> AVal.force
        //        |> HashMap.toList
        //        |> List.map (fun (k, v) -> HTMLAttr.Custom(k, v) :> IProp)
        //    let typ = Fable.React.ReactElementType.ofComponent<AdaptiveListComponent,_,_>
        //    Fable.React.ReactElementType.create typ { tag = tag; attributes = props; children = children } []
        else
            let typ = Fable.React.ReactElementType.ofComponent<AdaptiveComponent,_,_>
            Fable.React.ReactElementType.create typ { tag = tag; attributes = attributes; children = children } []
        