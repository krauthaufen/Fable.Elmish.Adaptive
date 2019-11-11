namespace Fable.React.Adaptive

open System
open Browser
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.ReactiveComponents
open Fable.React.Props
open FSharp.Data.Adaptive
open Fable.JsHelpers

type internal AdaptiveComponentProps =
    {   
        tag         : string
        attributes  : AttributeMap
        children    : alist<ReactElement>
    }

type internal AdaptiveComponentState(tag : string, attributes : AttributeMap, children : alist<ReactElement>) =
    static let noDisposable =
        { new IDisposable with member x.Dispose() = () }

    let mutable tag = tag
    let mutable children = children
    let mutable element : Types.Element = null
    let mutable parent : Types.Element = null
    let mutable reader : Option<IIndexListReader<ReactElement>> = None
    let mutable marking = noDisposable
    let mutable cache : IndexList<ReactPseudoParent> = IndexList.empty

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
        promise {
            for (index, op) in IndexListDelta.toSeq deltas do
                match op with
                | Set react ->  
                    let (_l, self, right) = IndexList.neighbours index cache
                    match self with
                    | Some (_, dst) ->  
                        let old = dst.Current
                        let! n = dst.Render(react)
                        if old <> n then element.replaceChild(n, old) |> ignore

                    | None ->
                        match right with
                        | Some (_ri, re) ->
                            let parent = ReactPseudoParent()
                            let! node = parent.Render(react)
                            let re = re.Current
                            element.insertBefore(node, re) |> ignore
                            cache <- IndexList.set index parent cache

                        | None ->
                            let parent = ReactPseudoParent()
                            let! node = parent.Render(react)
                            element.appendChild node |> ignore
                            cache <- IndexList.set index parent cache

                | Remove ->
                    match IndexList.tryRemove index cache with
                    | Some (root, rest) ->
                        cache <- rest
                        match! root.Unmount() with
                        | Some e -> element.removeChild e |> ignore
                        | None -> ()

                    | None ->
                        ()
        } |> ignore

    member x.Children = children
    member x.Tag = tag

    member x.SetElement (e : Types.Element) =
        if isNull e then 
            ()
        elif isNull element || element = e then
            element <- e
            parent <- e.parentElement
            match att with
            | Some att -> att.SetNode e
            | None -> ()
        else
            promise {
                for r in cache do
                    let! c = r.Element
                    e.appendChild c |> ignore
                    ()
            } |> ignore

            tag <- e.tagName.ToLower()
            element <- e
            parent <- e.parentElement
            match att with
            | Some att -> att.SetNode e
            | None -> ()

    member x.Replace(newList : alist<ReactElement>, callback : unit -> unit) =
        if children = newList then
            let r = getReader callback
            update (r.GetChanges AdaptiveToken.Top)
        else
            children <- newList
            match reader with
            | Some oldReader ->
                let newReader = recreateReader callback
                newReader.GetChanges AdaptiveToken.Top |> ignore
                let changes = IndexList.computeDelta oldReader.State newReader.State
                update changes
            | None ->
                let r = getReader callback
                update (r.GetChanges AdaptiveToken.Top)
      
    member x.ReplaceAttributes(newAttributes : AttributeMap, callback : unit -> unit) =
        let up = getAttributeUpdater callback
        if attributes <> newAttributes then
            attributes <- newAttributes
            up.SetAttributes newAttributes
        up.Update AdaptiveToken.Top

            


    member x.Update(callback : unit -> unit) =
        let r = getReader callback
        let u = getAttributeUpdater callback
        let res = update (r.GetChanges AdaptiveToken.Top)
        u.Update AdaptiveToken.Top
        res

    member x.Dispose() =
        for e in cache do 
            e.Unmount().``then``(function 
                | Some e -> element.removeChild e |> ignore 
                | None -> ()
            ) |> ignore

        match att with
        | Some a -> a.Destroy()
        | None -> ()

        marking.Dispose()
        attMarking.Dispose()
        children <- AList.empty
        element <- null
        reader <- None
        marking <- noDisposable
        cache <- IndexList.empty
        attributes <- AMap.empty
        attMarking <- noDisposable
        att <- None

type internal AdaptiveComponent(a : AdaptiveComponentProps)  =
    inherit Fable.React.Component<AdaptiveComponentProps, State<AdaptiveComponentState>>(a) 
    static do ComponentHelpers.setDisplayName<AdaptiveComponent,_,_> "AList"
    do base.setInitState({ value = AdaptiveComponentState(a.tag, a.attributes, a.children) })
       
    member x.invalidate() =
        Timeout.set 0 x.forceUpdate |> ignore

    member x.state = 
        base.state.value

    override x.componentDidMount() =
        x.state.Update x.invalidate |> ignore

    override x.componentWillUnmount() =
        x.state.Dispose()

    override x.componentDidUpdate(_, _) =
        x.state.Replace(x.props.children, x.invalidate) |> ignore
        x.state.ReplaceAttributes(x.props.attributes, x.invalidate)

    override x.shouldComponentUpdate(_,_) =
        true

    override x.render() =
        ReactBindings.React.createElement(
            x.props.tag, 
            keyValueList CaseRules.LowerFirst [Ref x.state.SetElement], 
            []
        )
  


type internal AdaptiveStringProps =
    {
        text : aval<string>
    }
    
type internal AdaptiveStringState(text : aval<string>, cb : unit -> unit) =
    let mutable sub = text.AddMarkingCallback(cb)
    let mutable text = text

    member x.Current : string =
        AVal.force text

    member x.SetText(t : aval<string>, callback : unit -> unit) =
        if text <> t then
            sub.Dispose()
            sub <- t.AddMarkingCallback(callback)
            text <- t
        
    member x.Dispose() =
        sub.Dispose()
        text <- AVal.constant ""

type internal AdaptiveStringComponent(a : AdaptiveStringProps) as this  =
    inherit Fable.React.Component<AdaptiveStringProps, State<AdaptiveStringState>>(a) 
    static do ComponentHelpers.setDisplayName<AdaptiveComponent,_,_> "AList"
    do base.setInitState({ value = AdaptiveStringState(a.text, this.invalidate) })
       
    member x.invalidate() =
        Timeout.set 0 x.forceUpdate |> ignore

    member x.state = 
        base.state.value

    override x.componentWillUnmount() =
        x.state.Dispose()

    override x.componentDidUpdate(_, _) = 
        x.state.SetText(x.props.text, x.invalidate)

    override x.shouldComponentUpdate(_,_) =
        true

    override x.render() =
        str x.state.Current 

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
                attributes.Content
                |> AVal.force
                |> HashMap.toList
                |> List.map (fun (k, v) -> HTMLAttr.Custom(k, v))

            let typ = Fable.React.ReactElementType.ofHtmlElement tag
            Fable.React.ReactElementType.create typ (keyValueList CaseRules.LowerFirst props) children
            
        else
            let typ = Fable.React.ReactElementType.ofComponent<AdaptiveComponent,_,_>
            Fable.React.ReactElementType.create typ { tag = tag; attributes = attributes; children = children } []
        