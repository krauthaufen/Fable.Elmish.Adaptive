namespace Fable.React.Adaptive

open System
open Browser
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.ReactiveComponents
open Fable.React.Props
open FSharp.Data.Adaptive


type internal ReactPseudoParent() =
    [<Emit("Object.defineProperty($0, $1, { get: $2 })")>]
    static let defineProperty (o : obj) (name : string) (getter : unit -> 'a) : unit = jsNative

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
        defineProperty e "firstChild" (fun () -> child)

        // fake appendChild (setting our one and only child)
        e?appendChild <- fun (n : Types.Node) ->
            child <- n
            defineProperty n "parentNode" (fun () -> e)

            //let getter = n?__proto__?__lookupGetter__("parentNode")
            //defineProperty n "realParentNode" (fun () ->
            //    getter?call(n)
            //)

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
    let mutable cache : IndexList<ReactPseudoParent> = IndexList.empty

    let getReader(callback : unit -> unit) =
        match reader with
        | Some r -> r
        | None -> 
            let r = children.GetReader()
            marking <- r.AddMarkingCallback callback
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
                            let! re = re.Element
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
        }

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
        x.state.Update x.forceUpdate |> ignore

    override x.componentWillUnmount() =
        x.state.Dispose()

    override x.componentDidUpdate(_, _) =
        x.state.Replace(x.props.children, x.forceUpdate) |> ignore

    override x.shouldComponentUpdate(_,_) =
        true

    override x.render() =
        ReactBindings.React.createElement(
            x.props.tag, 
            keyValueList CaseRules.LowerFirst [Ref x.state.SetElement], 
            []
        )
          

module AListComponent =
    let ofAList (tag : string) (children : alist<ReactElement>) =
        if children.IsConstant then
            let children =
                children.Content
                |> AVal.force
                |> IndexList.toList

            let typ = Fable.React.ReactElementType.ofHtmlElement tag
            Fable.React.ReactElementType.create typ () children
            
        else
            let typ = Fable.React.ReactElementType.ofComponent<AListComponent,_,_>
            Fable.React.ReactElementType.create typ { tag = tag; children = children } []
        