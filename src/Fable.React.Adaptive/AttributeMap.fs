namespace Fable.React.Adaptive

open Fable.Core
open Fable.React.Props
open Fable.Core.JsInterop
open FSharp.Data.Adaptive
open Fable.React.Adaptive.JsHelpers
open Browser
open FSharp.Data.Traceable

#if !BLA

type AttributeMapReader(constant : HashMap<string, Index * obj>, adaptive : IndexList<amap<string, obj>>) =
    inherit AbstractReader<HashMapDelta<string, obj>>(HashMapDelta.empty)

    static let resolve (key : string) (l : obj) (r : obj) =
        if key = "className" then
            string l + " " + string r |> box

        elif JsType.isFunction l && JsType.isFunction r then
            let l = unbox<JsFunc> l
            let r = unbox<JsFunc> r
            box <| fun () ->
                l.Invoke arguments |> ignore
                r.Invoke arguments |> ignore
        elif JsType.isObject l && JsType.isObject r then
            let res = obj()
            res.Assign(l)
            res.Assign(r)
            res
        else
            r

    let readers = 
        adaptive |> IndexList.mapi (fun p a -> 
            let r = a.GetReader()
            r.Tag <- p
            r
        )
    let mutable initial = true
    let mutable dirty = readers

    let mutable state : HashMap<string, IndexList<obj>> = HashMap.empty
    let mutable resultState : HashMap<string, obj> = HashMap.empty

    member x.State = resultState

    override x.InputChangedObject(t, o) =
        if unbox o.Tag then dirty <- IndexList.set (unbox o.Tag) (unbox o) dirty

    override x.Compute(token : AdaptiveToken) = 
        let d = dirty
        dirty <- IndexList.empty
        let dirty = d

        let mutable deltas = 
            if initial then
                initial <- false
                state <- constant |> HashMap.map (fun _ (p, v) -> IndexList.empty |> IndexList.set p v)
                state |> HashMap.map (fun _ v -> Set v) 
            else
                HashMap.empty

        for (p, r) in IndexList.toSeqIndexed dirty do
            let delta = r.GetChanges token |> HashMapDelta.toHashMap

            let newSets = 
                delta |> HashMap.map (fun k op ->
                    match op with
                    | Set v -> 
                        match HashMap.tryFind k state with
                        | Some set ->
                            let set = IndexList.set p v set
                            state <- HashMap.add k set state
                            Set set
                        | None ->
                            let set = IndexList.empty |> IndexList.set p v
                            state <- HashMap.add k set state
                            Set set
                    | Remove ->
                        match HashMap.tryFind k state with
                        | Some set ->
                            let set = IndexList.remove p set
                            if set.IsEmpty then Remove
                            else Set set
                        | None ->
                            Remove
                )

            let merge (k : string) (l : ElementOperation<IndexList<obj>>) (r : ElementOperation<IndexList<obj>>) =
                match l, r with
                | Remove, Remove -> Remove
                | Set l, Set r -> Set ((l, IndexList.toSeqIndexed r) ||> Seq.fold (fun l (i,r) -> IndexList.set i r l))
                | Set l, Remove -> Set l
                | Remove, Set r -> Set r

            deltas <- HashMap.unionWith merge deltas newSets


        deltas |> HashMap.map (fun k op ->
            match op with
            | Set set ->
                match IndexList.tryRemove set.MinIndex set with
                | Some (fst, set) -> 
                    let res = (fst, set) ||> Seq.fold (resolve k)
                    resultState <- HashMap.add k res resultState
                    Set res
                | None ->
                    resultState <- HashMap.remove k resultState
                    Remove
            | Remove ->
                resultState <- HashMap.remove k resultState
                Remove
        ) |> HashMapDelta.ofHashMap

type AttributeMap(isConstant : bool, content : IndexList<Choice<amap<string, obj>, HashMap<string, obj>>>) =
    
    static let empty = AttributeMap(true, IndexList.empty)

    static let resolveOrdered (key : string) (l : obj) (r : obj) =
        if key = "className" then
            let res = string l + " " + string r |> box
            res

        elif JsType.isFunction l && JsType.isFunction r then
            let l = unbox<JsFunc> l
            let r = unbox<JsFunc> r
            let res = 
                box <| fun () ->
                    l.Invoke arguments |> ignore
                    r.Invoke arguments |> ignore
            res
        elif JsType.isObject l && JsType.isObject r then
            let res = obj()
            res.Assign(l)
            res.Assign(r)
            res
        else
            r

    static let resolve (key : string) (li : Index) (ri : Index) (l : obj) (r : obj) =
        if li > ri then li, resolveOrdered key r l
        else ri, resolveOrdered key l r

    static let addConstant (m : HashMap<string, obj>) (l : IndexList<Choice<amap<string, obj>, HashMap<string, obj>>>) =
        match IndexList.tryLast l with
        | Some (Choice2Of2 o) ->
            IndexList.set l.MaxIndex (Choice2Of2 (HashMap.unionWith resolveOrdered o m)) l
        | _ ->
            IndexList.add (Choice2Of2 m) l

    static member Empty = empty

    member x.Store = content

    member x.Content = 
        let constants = content |> IndexList.toSeq |> Seq.map ( function Choice2Of2 c -> c | Choice1Of2 c -> AMap.force c)
        (HashMap.empty, constants) ||> Seq.fold (fun m c ->
            let mutable m : HashMap<string, obj> = m
            for (k,v) in c do
                m <- m |> HashMap.alter k (fun o ->
                    match o with
                    | Some ov -> resolveOrdered k ov v |> Some
                    | None -> v |> Some
                )
            m
        )

    member x.IsConstant = isConstant

    member x.GetReader() =  
        let constants =
            let constants = content |> IndexList.toSeqIndexed |> Seq.choose ( function (i,Choice2Of2 c) -> Some (i,c) | _ -> None)
            (HashMap.empty, constants) ||> Seq.fold (fun m (i,c) ->
                let mutable m : HashMap<string, Index * obj> = m
                for (k,v) in c do
                    m <- m |> HashMap.alter k (fun o ->
                        match o with
                        | Some (oi, ov) -> resolve k oi i ov v |> Some
                        | None -> (i, v) |> Some
                    )
                m
            )

        let adaptives =
            content |> IndexList.choose ( function Choice1Of2 c -> Some c | _ -> None)

        AttributeMapReader(constants, adaptives)
        
    member x.Add(m : HashMap<string, obj>) =
        AttributeMap(x.IsConstant, addConstant m content)

    member x.Add(m : amap<string, obj>) =
        if m.IsConstant then AttributeMap(x.IsConstant, addConstant (AMap.force m) content)
        else AttributeMap(false, IndexList.add (Choice1Of2 m) content)

    new (m : HashMap<string, obj>) =
        AttributeMap(true, IndexList.single (Choice2Of2 m))

    new (m : amap<string, obj>) =
        if m.IsConstant then AttributeMap(true, IndexList.single (Choice2Of2 (AMap.force m)))
        else AttributeMap(false, IndexList.single (Choice1Of2 m))
        

    static member Union(l : AttributeMap, r : AttributeMap) =
        let c = IndexList.append l.Store r.Store
        AttributeMap(l.IsConstant && r.IsConstant, c)

module AttributeMap =
    [<Emit("$0.charAt(0).toLowerCase() + $0.slice(1)")>]
    let private lowerFirst (str : string) : string = jsNative

    let private resolve (key : string) (l : obj) (r : obj) =
        if key = "className" then
            string l + " " + string r |> box

        elif JsType.isFunction l && JsType.isFunction r then
            let l = unbox<JsFunc> l
            let r = unbox<JsFunc> r
            box <| fun () ->
                l.Invoke arguments |> ignore
                r.Invoke arguments |> ignore
        elif JsType.isObject l && JsType.isObject r then
            let res = obj()
            res.Assign(l)
            res.Assign(r)
            res
        else
            r
        
    let inline private toHashMap (p : seq<'T>) : HashMap<string, obj> =
        let mutable res = HashMap.empty

        for e in p do
            let name, value = 
                if e?name then
                    let name = e?name
                    let values : obj[] = e?fields
                    if values.Length > 1 then
                        unbox values.[0], values.[1]
                    else
                        name, values.[0]
                else
                    let arr = unbox<obj[]> e
                    string arr.[0], arr.[1]

            let name = lowerFirst name
            res <- res |> HashMap.alter name (function Some o -> Some (resolve name o value) | None -> Some value)

        res

    let empty : AttributeMap = AttributeMap.Empty

    let single (attr : IProp) : AttributeMap =
        toHashMap [| attr |] |> AttributeMap
        
    let ofAValSingle (attr : aval<#IProp>) : AttributeMap =
        attr
        |> AVal.map (fun a -> toHashMap [| a |])
        |> AMap.ofAVal
        |> AttributeMap
        
    let ofAValOption (attr : aval<option<#IProp>>) : AttributeMap =
        attr
        |> AVal.map (function Some a -> toHashMap [| a |] | _ -> HashMap.empty)
        |> AMap.ofAVal
        |> AttributeMap

    let ofAVal (attr : aval<#seq<#IProp>>) : AttributeMap =
        attr
        |> AVal.map (fun a -> toHashMap (a :> seq<_>))
        |> AMap.ofAVal
        |> AttributeMap
 
    let ofSeq (l : seq<#IProp>) : AttributeMap = 
        toHashMap l |> AttributeMap

    let ofList (l : list<#IProp>) : AttributeMap = 
        toHashMap l|> AttributeMap

    let ofArray (l : array<#IProp>) : AttributeMap = 
        toHashMap l |> AttributeMap
        
    let union (l : AttributeMap) (r : AttributeMap) : AttributeMap =
        AttributeMap.Union(l, r)

    let force (m : AttributeMap) = m.Content

    type Builder() =
        member inline x.Zero() = 
            empty

        member inline x.Yield (attr : IProp) = 
            single attr
             
        member inline x.Yield (attr : aval<#IProp>) =
            ofAValSingle attr

        member inline x.Yield (attr : aval<option<#IProp>>) =
            ofAValOption attr

        member inline x.Yield (attr : aval<seq<#IProp>>) =
            ofAVal attr
            
        member inline x.Yield (attr : aval<list<#IProp>>) =
            ofAVal attr
            
        member inline x.Yield (attr : aval<array<#IProp>>) =
            ofAVal attr


        member inline x.YieldFrom(attr : AttributeMap) =
            attr

        member inline x.Delay (action : unit -> AttributeMap) =
            action

        member inline x.Run(action : unit -> AttributeMap) =
            action()

        member inline x.For(elements : seq<'T>, mapping : 'T -> AttributeMap) =
            (empty, elements) ||> Seq.fold (fun s e -> union s (mapping e))
            

        member inline x.While(guard : unit -> bool, body : unit -> AttributeMap) =
            let mutable res = empty
            while guard() do
                res <- union res (body())
            res

        member inline x.Combine(l : AttributeMap, r : unit -> AttributeMap) =
            union l (r())

#else

type AttributeMap = amap<string, obj>

module AttributeMap =
    [<Emit("$0.charAt(0).toLowerCase() + $0.slice(1)")>]
    let private lowerFirst (str : string) : string = jsNative

    let private resolve (key : string) (l : obj) (r : obj) =
        if key = "className" then
            string l + " " + string r |> box

        elif JsType.isFunction l && JsType.isFunction r then
            let l = unbox<JsFunc> l
            let r = unbox<JsFunc> r
            box <| fun () ->
                l.Invoke arguments |> ignore
                r.Invoke arguments |> ignore
        elif JsType.isObject l && JsType.isObject r then
            let res = obj()
            res.Assign(l)
            res.Assign(r)
            res
        else
            r
        
    let inline private toHashMap (p : seq<'T>) : HashMap<string, obj> =
        let mutable res = HashMap.empty

        for e in p do
            let name, value = 
                if e?name then
                    let name = e?name
                    let values : obj[] = e?fields
                    if values.Length > 1 then
                        unbox values.[0], values.[1]
                    else
                        name, values.[0]
                else
                    let arr = unbox<obj[]> e
                    string arr.[0], arr.[1]

            let name = lowerFirst name
            res <- res |> HashMap.alter name (function Some o -> Some (resolve name o value) | None -> Some value)

        res

    let empty : AttributeMap = AMap.empty<string, obj>

    let single (attr : IProp) : AttributeMap =
        toHashMap [| attr |] |> AMap.ofHashMap
        
    let ofAValSingle (attr : aval<#IProp>) : AttributeMap =
        attr
        |> AVal.map (fun a -> toHashMap [| a |])
        |> AMap.ofAVal
        
    let ofAValOption (attr : aval<option<#IProp>>) : AttributeMap =
        attr
        |> AVal.map (function Some a -> toHashMap [| a |] | _ -> HashMap.empty)
        |> AMap.ofAVal

    let ofAVal (attr : aval<#seq<#IProp>>) : AttributeMap =
        attr
        |> AVal.map (fun a -> toHashMap (a :> seq<_>))
        |> AMap.ofAVal
 
    let ofSeq (l : seq<#IProp>) : AttributeMap = 
        toHashMap l |> AMap.ofHashMap

    let ofList (l : list<#IProp>) : AttributeMap = 
        toHashMap l |> AMap.ofHashMap

    let ofArray (l : array<#IProp>) : AttributeMap = 
        toHashMap l |> AMap.ofHashMap
        
    let union (l : AttributeMap) (r : AttributeMap) : AttributeMap =
        AMap.unionWith resolve l r

    let force (m : AttributeMap) = AMap.force m

    type Builder() =
        member inline x.Zero() = 
            empty

        member inline x.Yield (attr : IProp) = 
            single attr
             
        member inline x.Yield (attr : aval<#IProp>) =
            ofAValSingle attr

        member inline x.Yield (attr : aval<option<#IProp>>) =
            ofAValOption attr

        member inline x.Yield (attr : aval<seq<#IProp>>) =
            ofAVal attr
            
        member inline x.Yield (attr : aval<list<#IProp>>) =
            ofAVal attr
            
        member inline x.Yield (attr : aval<array<#IProp>>) =
            ofAVal attr


        member inline x.YieldFrom(attr : AttributeMap) =
            attr

        member inline x.Delay (action : unit -> AttributeMap) =
            action

        member inline x.Run(action : unit -> AttributeMap) =
            action()

        member inline x.For(elements : seq<'T>, mapping : 'T -> AttributeMap) =
            (empty, elements) ||> Seq.fold (fun s e -> union s (mapping e))
            

        member inline x.While(guard : unit -> bool, body : unit -> AttributeMap) =
            let mutable res = empty
            while guard() do
                res <- union res (body())
            res

        member inline x.Combine(l : AttributeMap, r : unit -> AttributeMap) =
            union l (r())

#endif

[<AutoOpen>]
module AttributeMapBuilder =
    let attr = AttributeMap.Builder()