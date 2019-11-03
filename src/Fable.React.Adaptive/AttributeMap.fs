namespace Fable.React.Adaptive

open Fable.Core
open Fable.React.Props
open Fable.Core.JsInterop
open FSharp.Data.Adaptive


type AttributeMap = amap<string, obj>

module internal UnionType =

    [<Emit("Object.keys($0)")>]
    let keys (o : obj) : seq<string> = jsNative

    let inline toHashMap (rule : CaseRules) (p : seq<'T>) : HashMap<string, obj> =
        let o = keyValueList rule p
        keys o 
        |> Seq.map (fun k -> k, o?(k))
        |> HashMap.ofSeq

module AttributeMap =

    [<Emit("Object.assign($0, $1)")>]
    let private assign (dst : obj) (src : obj) : unit = jsNative

    
    [<Emit("typeof $0 === \"function\"")>]
    let private isFunction (o : obj) : bool = jsNative
    
    
    [<Emit("arguments")>]
    let private args : obj[] = jsNative



    let empty : AttributeMap = AMap.empty<string, obj>

    let single (attr : IHTMLProp) : AttributeMap =
        UnionType.toHashMap CaseRules.LowerFirst [| attr |] |> AMap.ofHashMap
        
    let ofAValSingle (attr : aval<#IHTMLProp>) : AttributeMap =
        attr
        |> AVal.map (fun a -> UnionType.toHashMap CaseRules.LowerFirst [| a |])
        |> AMap.ofAVal
        
    let ofAValOption (attr : aval<option<#IHTMLProp>>) : AttributeMap =
        attr
        |> AVal.map (function Some a -> UnionType.toHashMap CaseRules.LowerFirst [| a |] | _ -> HashMap.empty)
        |> AMap.ofAVal

    let ofAVal (attr : aval<#seq<#IHTMLProp>>) : AttributeMap =
        attr
        |> AVal.map (fun a -> UnionType.toHashMap CaseRules.LowerFirst (a :> seq<_>))
        |> AMap.ofAVal
             

    let ofSeq (l : seq<#IHTMLProp>) : AttributeMap = 
        UnionType.toHashMap CaseRules.LowerFirst l |> AMap.ofHashMap

    let ofList (l : list<#IHTMLProp>) : AttributeMap = 
        UnionType.toHashMap CaseRules.LowerFirst l |> AMap.ofHashMap

    let ofArray (l : array<#IHTMLProp>) : AttributeMap = 
        UnionType.toHashMap CaseRules.LowerFirst l |> AMap.ofHashMap
        
    let union (l : AttributeMap) (r : AttributeMap) : AttributeMap =
        let resolve (key : string) (l : obj) (r : obj) =
            match key with
            | "className" -> string l + " " + string r |> box
            | "style" ->
                let res = obj()
                assign res l
                assign res r
                res
            | _ -> 
                if isFunction l && isFunction r then
                    let l = unbox<JsFunc> l
                    let r = unbox<JsFunc> r
                    box <| fun () ->
                        l.Invoke args |> ignore
                        r.Invoke args |> ignore
                else
                    r
        AMap.unionWith resolve l r

    type Builder() =
        member inline x.Zero() = 
            empty

        member inline x.Yield (attr : IHTMLProp) = 
            single attr

        member inline x.Yield (attr : aval<#IHTMLProp>) =
            ofAValSingle attr

        member inline x.Yield (attr : aval<option<#IHTMLProp>>) =
            ofAValOption attr

        member inline x.Yield (attr : aval<seq<#IHTMLProp>>) =
            ofAVal attr
            
        member inline x.Yield (attr : aval<list<#IHTMLProp>>) =
            ofAVal attr
            
        member inline x.Yield (attr : aval<array<#IHTMLProp>>) =
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

[<AutoOpen>]
module AttributeMapBuilder =
    let attr = AttributeMap.Builder()