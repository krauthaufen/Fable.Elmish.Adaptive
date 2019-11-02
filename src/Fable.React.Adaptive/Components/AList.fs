namespace Fable.React.Adaptive

open System
open Browser
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.ReactiveComponents
open Fable.React.Props
open FSharp.Data.Adaptive



type internal ReactRenderedElement(insert : Types.Element -> JS.Promise<unit>, remove : Types.Element -> unit) =
    let stub = document.createElement "span"
    let dst = document.createElement "span"

    let setElement, element =
        let mutable success = Unchecked.defaultof<_>
        let res = Promise.create (fun s _ -> success <- s)
        success, res


    let mutable real : Option<Types.Element> = None
    let mutable last : Option<JS.Promise<unit>> = None

    let run (action : unit -> JS.Promise<'a>) =
        match last with
        | Some l ->
            let res = l |> Promise.bind action
            last <- Some (unbox res)
            res
        | None ->
            let res = action()
            last <- Some (unbox res)
            res

    member x.Element = element

    member x.Render(ui : ReactElement) =
        run (fun () ->
            Promise.create (fun success _error ->
                ReactDom.render(ReactDom.createPortal(ui, dst), stub, fun () ->
                    match real with
                    | Some real ->
                        success real

                    | None -> 
                        let e = unbox dst.firstChild
                        insert(e).``then`` (fun () ->
                            real <- Some e
                            setElement e
                            success e
                        ) |> ignore
                )
            )
        )

    member x.Unmount() =
        run (fun () ->
            Promise.create (fun success _error ->
                match real with
                | Some r ->
                    remove r
                    dst.appendChild r |> ignore
                    ReactDom.unmountComponentAtNode stub |> success
                | None ->
                    success false

                real <- None
            )
        )

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
    let mutable cache : IndexList<ReactRenderedElement> = IndexList.empty

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
                let (_l, self, right) = IndexList.neighbours index cache
                match self with
                | Some (_, dst) -> 
                    dst.Render(react) |> ignore

                | None ->
                    match right with
                    | Some (_ri, re) ->
                        let insert (e : Types.Element) =
                            re.Element |> Promise.map (fun re ->
                                element.insertBefore(e, re) |> ignore
                            )

                        let remove (e : Types.Element) =
                            e.remove()

                        let element = ReactRenderedElement(insert, remove)
                        element.Render(react) |> ignore
                        cache <- IndexList.set index element cache

                    | None ->
                        
                        let insert (e : Types.Element) =
                            promise {
                                element.appendChild(e) |> ignore
                            }

                        let remove (e : Types.Element) =
                            e.remove()

                        let element = ReactRenderedElement(insert, remove)
                        element.Render(react) |> ignore
                        cache <- IndexList.set index element cache

            | Remove ->
                match IndexList.tryRemove index cache with
                | Some (root, rest) ->
                    cache <- rest
                    root.Unmount() |> ignore

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
            promise {
                for r in cache do
                    let! c = r.Element
                    e.appendChild c |> ignore
                    ()
            } |> ignore

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
    static do ComponentHelpers.setDisplayName<AListComponent,_,_> "AList"
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
          

module AListComponent =
    let ofAlist (tag : string) (children : alist<ReactElement>) =
        let typ = Fable.React.ReactElementType.ofComponent<AListComponent,_,_>
        Fable.React.ReactElementType.create typ { tag = tag; children = children } []
        