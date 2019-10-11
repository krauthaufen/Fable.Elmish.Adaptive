namespace Fable.Elmish.Adaptive

open FSharp.Data.Adaptive
open FSharp.Data.Traceable
open Browser
open Browser.Types

module Updater = 

    type Scope =
        {
            subscribeAdaptive   : IAdaptiveObject -> (IAdaptiveObject -> AdaptiveToken -> obj) -> (obj -> unit) -> System.IDisposable
        }

        member x.subscribe(m : aval<'a>, callback : 'a -> unit) =
            x.subscribeAdaptive (m :> IAdaptiveObject) (fun m t -> (unbox<aval<'a>> m).GetValue(t) :> obj) (unbox >> callback)
         
        member x.subscribe(m : aset<'a>, callback : CountingHashSet<'a> -> HashSetDelta<'a> -> unit) =
            let r = m.GetReader()
            x.subscribeAdaptive 
                (r :> IAdaptiveObject) 
                (fun r t -> 
                    let r = (unbox<IHashSetReader<'a>> r)
                    let ops = r.GetChanges t
                    (r.State, ops) :> obj
                ) 
                (fun o ->
                    let (state, deltas) = unbox o
                    callback state deltas
                )
         
        member x.subscribe(m : amap<'k, 'v>, callback : HashMap<'k, 'v> -> HashMapDelta<'k, 'v> -> unit) =
            let r = m.GetReader()
            x.subscribeAdaptive 
                (r :> IAdaptiveObject) 
                (fun r t -> 
                    let r = (unbox<IHashMapReader<'k, 'v>> r)
                    let ops = r.GetChanges t
                    (r.State, ops) :> obj
                ) 
                (fun o ->
                    let (state, deltas) = unbox o
                    callback state deltas
                )
         
        member x.subscribe(m : alist<'a>, callback : IndexList<'a> -> IndexListDelta<'a> -> unit) =
            let r = m.GetReader()
            x.subscribeAdaptive 
                (r :> IAdaptiveObject) 
                (fun r t -> 
                    let r = (unbox<IIndexListReader<'a>> r)
                    let ops = r.GetChanges t
                    (r.State, ops) :> obj
                ) 
                (fun o ->
                    let (state, deltas) = unbox o
                    callback state deltas
                )
         
    type EventTarget with
        member x.subscribe(name : string, callback : Event -> unit, useCapture : bool) =
            x.addEventListener(name, callback, useCapture)
            { new System.IDisposable with
                member __.Dispose() = x.removeEventListener(name, callback, useCapture)
            }

    [<AbstractClass>] 
    type NodeUpdater(s : Scope) =
        inherit AdaptiveObject()

        static let create (scope : Scope) (createNode : Tag -> HTMLElement) (n : DomNode) =
            match n with
            | DomNode.Text(a,b,c) -> TextUpdater(scope, createNode(a), a, b, c) :> NodeUpdater
            | DomNode.Inner(a,b,c) -> InnerNodeUpdater(scope, createNode(a), a, b, c) :> NodeUpdater
            | DomNode.Void(a,b) -> TextUpdater(scope, createNode(a), a, b, AVal.constant "") :> NodeUpdater

        static member Create(parent : HTMLElement, scope : Scope, n : DomNode) =
            let createNode (tag : Tag) =
                let n = 
                    match tag.xmlns with
                    | Some ns -> document.createElementNS(ns, tag.name) |> unbox<HTMLElement>
                    | None -> document.createElement(tag.name)

                let arr = FSharp.Collections.Array.init (int parent.children.length) (fun i -> parent.children.[i])
                for c in arr do c.remove()
                parent.appendChild n |> ignore
                n
            create scope createNode n
            
        static member Create(createNode : Tag -> HTMLElement, scope : Scope, n : DomNode) =
            create scope createNode n

        member x.Scope = s

        abstract member Node : HTMLElement
        abstract member Compute : AdaptiveToken -> unit
        abstract member Kill : unit -> unit
        abstract member TryReplace : AdaptiveToken * DomNode -> bool


        member x.Update(t : AdaptiveToken) =
            x.EvaluateIfNeeded t () (fun t ->
                x.Compute(t)
            )

        member x.Destroy() =
            x.Outputs.Consume() |> ignore
            x.Kill()

    and AttributeUpdater(node : HTMLElement, eventTarget : EventTarget, attributes : AttributeMap) =
        inherit AdaptiveObject()
        let mutable attributes = attributes
        let mutable reader = attributes.Store.GetReader()
        let listeners = UncheckedDictionary.create<string, array<System.IDisposable>>() // Dict<string, array<System.IDisposable>>(Unchecked.hash, Unchecked.equals)
        let mutable shutdown : list<System.IDisposable> = []

        let update (ops : HashMapDelta<string, AttributeValue>) (s : Scope) =
            for (k, o) in ops do
                match o with
                | Remove ->
                    match listeners.TryGetValue k with
                    | (true, id) ->
                        listeners.Remove k |> ignore
                        for d in id do d.Dispose()
                    | _ -> 
                        node.removeAttribute k

                | Set value ->
                    match value with
                    | AttributeValue.String v -> 
                        node.setAttribute(k, v)
            
                    | AttributeValue.Event callbacks ->
                        let o =
                            match listeners.TryGetValue k with
                            | (true, o) -> Some o
                            | _ -> None
                        
                        let n =
                            match o with
                            | Some o -> o |> FSharp.Collections.Array.iter (fun d -> d.Dispose())
                            | None -> ()

                            let disposables = 
                                callbacks |> Seq.toArray |> FSharp.Collections.Array.map (fun cb ->
                                    let callback = cb.callback
                                    eventTarget.subscribe(k, (fun e -> callback(e)), cb.useCapture)         
                                )

                            disposables

                        listeners.[k] <- n



        member x.Replace(newAttributes : AttributeMap, t : AdaptiveToken, s : Scope) =
            x.EvaluateAlways t (fun t ->
                let newReader = newAttributes.Store.GetReader()
                let _ = newReader.GetChanges t
                let ops = HashMap.computeDelta reader.State newReader.State

                attributes <- newAttributes
                reader <- newReader
                shutdown |> List.iter (fun d -> d.Dispose())
                shutdown <- []
                update ops s
            )

        member x.UpdateAttributes(t : AdaptiveToken, s : Scope) =
            x.EvaluateIfNeeded t () (fun t -> 
                let ops = reader.GetChanges t
                update ops s
            )

        member x.Destroy() =
            for (k, _) in reader.State do
                match listeners.TryGetValue k with
                | (true, disp) ->
                    disp |> FSharp.Collections.Array.iter (fun d -> d.Dispose())
                | _ -> 
                    node.removeAttribute k
            listeners.Clear()
            reader <- Unchecked.defaultof<_>

    and TextUpdater(scope : Scope, node : HTMLElement, tag : Tag, attributes : AttributeMap, content : aval<string>) =
        inherit NodeUpdater(scope)

        let mutable attributes = attributes
        let mutable content = content

        let mutable lastValue = None
        let att = AttributeUpdater(node, node, attributes)
        
        override x.Node = node
        override x.Compute(t) =
            let v = content.GetValue t
            match lastValue with
            | Some o when o = v -> ()
            | _ ->
                node.innerText <- v
                lastValue <- Some v
            
            att.UpdateAttributes(t, scope)

        override x.Kill() =
            att.Destroy()
            node.innerText <- ""
            lastValue <- None
            
        override x.TryReplace (t : AdaptiveToken, n : DomNode) =
            match n with
            | DomNode.Text(nt, a, c) when nt = tag ->
                x.EvaluateAlways t (fun t ->
                    att.Replace(a, t, scope)
                    attributes <- a

                    content.Outputs.Remove x |> ignore
                    content <- c
                    let v = c.GetValue t
                    match lastValue with
                    | Some o when o = v -> ()
                    | _ ->
                        //Log.warn "repl %s.innerText = \"%s\"" node.tagName v
                        node.innerText <- v
                        lastValue <- Some v

                    true
                )
            | _ ->
                false

    and InnerNodeUpdater(scope : Scope, node : HTMLElement, tag : Tag, attributes : AttributeMap, children : alist<DomNode>) =
        inherit NodeUpdater(scope)

        let mutable attributes = attributes
        let mutable children = children
        let mutable reader = children.GetReader()

        let mutable nodes : IndexList<NodeUpdater> = IndexList.empty
        let att = AttributeUpdater(node, node, attributes)
        
        let mutable dirty = ReferenceHashSet.create()

        let update (t : AdaptiveToken) (ops : IndexListDelta<DomNode>) =
            let dirty =
                let d = dirty
                dirty <- ReferenceHashSet.create()
                d

            for (i, op) in IndexListDelta.toSeq ops do
                match op with
                | Remove ->
                    match IndexList.tryRemove i nodes with
                    | Some (u, rest) ->
                        dirty.Remove u |> ignore
                        nodes <- rest
                        u.Destroy()
                        if unbox u.Node then
                            u.Node.remove()
                    | None ->
                        ()

                | Set value ->
                    let (_l, s, r) = IndexList.neighbours i nodes

                    let insert (tag : Tag) =
                        match r with
                        | Some (_ri, r) when unbox r.Node ->
                            let n = 
                                match tag.xmlns with
                                | Some ns -> document.createElementNS(ns, tag.name) |> unbox<HTMLElement>
                                | None -> document.createElement(tag.name)
                            node.insertBefore(n, r.Node) |> ignore
                            n
                                
                        | _ ->
                            let n = 
                                match tag.xmlns with
                                | Some ns -> document.createElementNS(ns, tag.name) |> unbox<HTMLElement>
                                | None -> document.createElement(tag.name)
                            node.appendChild n |> ignore
                            n

                    match s with
                    | Some (_,s) ->
                        if not (s.TryReplace(t, value)) then
                            s.Destroy()
                            if unbox s.Node then s.Node.remove()
                            let n = NodeUpdater.Create(insert, scope, value)
                            n.Update(t)
                            nodes <- IndexList.set i n nodes

                    | None ->
                        let n = NodeUpdater.Create(insert, scope, value)
                        n.Update(t)
                        nodes <- IndexList.set i n nodes

            for s in dirty do
                s.Update(t)

        override x.InputChangedObject(t : obj, inner : IAdaptiveObject) =
            if not (System.Object.ReferenceEquals(inner, att)) then 
                dirty.Add (unbox<NodeUpdater> inner) |> ignore
            ()

        override x.Node = node
        override x.Compute(t) =
            let ops = reader.GetChanges t
            update t ops
            att.UpdateAttributes(t, scope)
                
            
        override x.Kill() =
            att.Destroy()

            for v in IndexList.toSeq nodes do
                v.Destroy()
                if unbox v.Node then v.Node.remove() 

            nodes <- IndexList.empty
            reader <- Unchecked.defaultof<_>


        override x.TryReplace (t : AdaptiveToken, n : DomNode) =
            match n with
            | DomNode.Inner(ntag, natt, nchildren) when tag = ntag ->
                x.EvaluateAlways t (fun t ->
                    att.Replace(natt, t, scope)
                    attributes <- natt

                    let r = nchildren.GetReader()
                    let _ = r.GetChanges t
                    let nState = r.State
                    let oState = nodes

                    let tryUpdate (key : Index) (o : Option<NodeUpdater>) (n : Option<DomNode>) =
                        match o, n with
                        | Some o, Some n -> 
                            if o.TryReplace(t, n) then
                                None
                            else
                                Some (ElementOperation.Set n)
                        | None, Some n ->
                            Some (ElementOperation.Set n)
                        | Some o, None ->
                            nodes <- IndexList.remove key nodes
                            o.Destroy()
                            Some (ElementOperation.Remove)
                        | None, None ->
                            None

                    let deltas = 
                        IndexList.choose2 tryUpdate oState nState 
                        |> IndexList.toSeqIndexed
                        |> IndexListDelta.ofSeq

                    children <- nchildren
                    reader <- r

                    update t deltas
                    true
                )
            | _ ->
                false

 
    let create (parent : HTMLElement) (scope : Scope) (node : DomNode) =
        NodeUpdater.Create(parent, scope, node)
