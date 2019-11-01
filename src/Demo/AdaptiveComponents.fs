namespace React.Adaptive

open System
open Browser
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.ReactiveComponents
open Fable.React.Props
open FSharp.Data.Adaptive
open Fable.Core.JsInterop

module private Helpers =
    let inline setDisplayName<'comp, 'props, 'state when 'comp :> Component<'props, 'state>> (name : string) =
        let t = Fable.React.ReactElementType.ofComponent<'comp, 'props, 'state>
        t?displayName <- name

    
type internal AListComponentProps =
    {   
        tag         : string
        children    : alist<ReactElement>
    }

type internal AListComponentState(tag : string, children : alist<ReactElement>) =
    static let noDisposable =
        { new IDisposable with member x.Dispose() = () }

    let mutable tag = tag
    let mutable children = children
    let mutable element : Types.Element = null
    let mutable parent : Types.Element = null
    let mutable reader : Option<IIndexListReader<ReactElement>> = None
    let mutable marking = noDisposable
    let mutable cache : IndexList<Types.HTMLElement * Types.Element> = IndexList.empty

    let getReader(callback : unit -> unit) =
        match reader with
        | Some r -> r
        | None -> 
            let r = children.GetReader()
            marking <- r.AddMarkingCallback callback
            reader <- Some r
            r

    let update (deltas : IndexListDelta<ReactElement>) =
        for (index, op) in IndexListDelta.toSeq deltas do
            match op with
            | Set react ->  
                let tag = "span"
                let (_l, self, right) = IndexList.neighbours index cache
                match self with
                | Some (_, (dst, a)) -> 
                    ReactDom.render(react, dst)

                | None ->
                    match right with
                    | Some (_ri, (_rep, re)) ->
                        let dummy = document.createElement tag
                        element.insertBefore(dummy, re) |> ignore
                        cache <- IndexList.set index (dummy, dummy :> _) cache
                        ReactDom.render(react, dummy)
                        //ReactDom.render(react, dummy, fun () ->
                        //    let actual = dummy :> Types.Element //if dummy.children.length = 1 then dummy.children.[0] else dummy :> _
                        //    element.insertBefore(actual, re) |> ignore
                        //    cache <- IndexList.set index (dummy, actual) cache
                        //)
                    | None ->
                        let dummy = document.createElement tag
                        element.appendChild(dummy) |> ignore
                        cache <- IndexList.set index (dummy, dummy :> _) cache
                        ReactDom.render(react, dummy)
                        //ReactDom.render(react, dummy, fun () ->
                        //    let actual = dummy :> Types.Element//if dummy.children.length = 1 then dummy.children.[0] else dummy :> _
                        //    element.appendChild(actual) |> ignore
                        //    cache <- IndexList.set index (dummy, actual) cache
                        //)

            | Remove ->
                match IndexList.tryRemove index cache with
                | Some ((dummy, actual), rest) ->
                    cache <- rest
                    if (dummy :> Types.Element) <> actual then
                        actual.remove()
                        dummy.appendChild actual |> ignore
                    let worked = ReactDom.unmountComponentAtNode dummy 
                    if not worked then
                        console.warn("could not unmount", dummy)
                | None ->
                    ()

    let recreateReader(callback : unit -> unit) =
        let r = children.GetReader()
        let s = r.AddMarkingCallback callback
        marking.Dispose()
        marking <- s
        reader <- Some r
        r

    member x.Children = children
    member x.Tag = tag

    member x.SetElement (e : Types.Element) =
        if isNull element || element = e then
            element <- e
            parent <- e.parentElement
        elif isNull e then
            ()
        else
            for (r, _) in cache do
               e.appendChild r |> ignore
                
            tag <- e.tagName.ToLower()
            element <- e
            parent <- e.parentElement


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
      
    member x.Update(callback : unit -> unit) =
        let r = getReader callback
        update (r.GetChanges AdaptiveToken.Top)

    member x.Dispose() =
        marking.Dispose()
        children <- AList.empty
        element <- null
        reader <- None
        marking <- noDisposable
        cache <- IndexList.empty

type internal AListComponent(a : AListComponentProps)  =
    inherit Fable.React.Component<AListComponentProps, State<AListComponentState>>(a) 
    static do Helpers.setDisplayName<AListComponent,_,_> "AList"
    do base.setInitState({ value = AListComponentState(a.tag, a.children) })
       
    member x.state = 
        base.state.value

    override x.componentDidMount() =
        x.state.Update x.forceUpdate

    override x.componentWillUnmount() =
        x.state.Dispose()

    override x.componentDidUpdate(_, _) =
        x.state.Replace(x.props.children, x.forceUpdate)

    override x.shouldComponentUpdate(_,_) =
        true

    override x.render() =
        ReactBindings.React.createElement(
            x.props.tag, 
            keyValueList CaseRules.LowerFirst [Ref x.state.SetElement], 
            []
        )
          

        
[<AutoOpen>]
module AdaptiveTags =

    let adaptiveNode (tag : string) (children : alist<ReactElement>) = 
        let typ = Fable.React.ReactElementType.ofComponent<AListComponent,_,_>
        Fable.React.ReactElementType.create typ { tag = tag; children = children } []

    let adiv c = adaptiveNode "div" c
    let aol c = adaptiveNode "ol" c
    let aul c = adaptiveNode "ul" c



module DebugComponents = 
    open Fable.React
    open Fable.Core.JsInterop
    open Fable.React.ReactiveComponents

    type LogComponent(value : State<string * ReactElement>) =
        inherit Component<State<string * ReactElement>, State<string * ReactElement>>(value)
        static do Helpers.setDisplayName<LogComponent,_,_> "Log"
        do base.setInitState { value = value.value }

        override x.componentWillUnmount() =
            let (name, _) = x.state.value
            console.log(name, "will unmount")

        override x.componentDidMount() =
            let (name, _) = x.state.value
            console.log(name, "did mount")

        override x.componentDidUpdate(_, _) =
            let (name, _) = x.state.value
            if x.state.value <> x.props.value then
                x.setState(fun _ _ -> { value = x.props.value })
            console.log(name, "did update")
            

        override x.componentDidCatch(e, info) =
            let (name, _) = x.state.value
            console.log(name, "did catch", e, info)
            
        override x.shouldComponentUpdate(np, ns) =
            let (name, _) = x.state.value
            console.log(name, "should update")
            true

        override x.render() =
            let (name, content) = x.state.value
            console.log(name, "render")
            content
            
    let withLogging name content = 
        ofType<LogComponent, _, _> { value = (sprintf "%s: " name, content) } []

