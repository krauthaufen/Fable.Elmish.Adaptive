namespace Fable.React.Adaptive

open Fable.Core
open Fable.React.Props
open Fable.Core.JsInterop
open FSharp.Data.Adaptive
open Fable.JsHelpers
open Browser


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

    let single (attr : IHTMLProp) : AttributeMap =
        toHashMap [| attr |] |> AMap.ofHashMap
        
    let ofAValSingle (attr : aval<#IHTMLProp>) : AttributeMap =
        attr
        |> AVal.map (fun a -> toHashMap [| a |])
        |> AMap.ofAVal
        
    let ofAValOption (attr : aval<option<#IHTMLProp>>) : AttributeMap =
        attr
        |> AVal.map (function Some a -> toHashMap [| a |] | _ -> HashMap.empty)
        |> AMap.ofAVal

    let ofAVal (attr : aval<#seq<#IHTMLProp>>) : AttributeMap =
        attr
        |> AVal.map (fun a -> toHashMap (a :> seq<_>))
        |> AMap.ofAVal
 
    let ofSeq (l : seq<#IHTMLProp>) : AttributeMap = 
        toHashMap l |> AMap.ofHashMap

    let ofList (l : list<#IHTMLProp>) : AttributeMap = 
        toHashMap l |> AMap.ofHashMap

    let ofArray (l : array<#IHTMLProp>) : AttributeMap = 
        toHashMap l |> AMap.ofHashMap
        
    let union (l : AttributeMap) (r : AttributeMap) : AttributeMap =
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