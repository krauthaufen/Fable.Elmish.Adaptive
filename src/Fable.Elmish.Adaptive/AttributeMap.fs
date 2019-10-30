namespace Fable.Elmish.Adaptive

open Browser
open Browser.Types
open FSharp.Data.Adaptive

type EventDescription =
    {
        useCapture  : bool
        callback    : Event -> unit
    }

[<RequireQualifiedAccess>]
type AttributeValue =
    | String of value : string
    | Event of list<EventDescription>

type AttributeMap(store : amap<string, AttributeValue>) =
    member x.Store = store

module AttributeMap =
    [<AutoOpen>]
    module private Helpers = 
        let mergeValue (attName : string) (l : string) (r : string) =
            if attName = "class" then sprintf "%s %s" l r
            elif attName = "style" then sprintf "%s;%s" l r
            else r
    
        let mergeAttributeValues (attName : string) (l : AttributeValue) (r : AttributeValue) =
            match l with
            | AttributeValue.Event le ->
                match r with
                | AttributeValue.Event re -> AttributeValue.Event (le @ re)
                | _ -> r
            | AttributeValue.String lv ->
                match r with
                | AttributeValue.String rv -> AttributeValue.String (mergeValue attName lv rv)
                | _ -> r

    let empty = 
        AttributeMap(AMap.empty)

    let single (name : string) (value : AttributeValue) =
        AMap.single name value |> AttributeMap

    let ofSeq (values : seq<string * AttributeValue>) =
        let mutable store = HashMap.empty
        for (name, v) in values do
            store <- 
                store |> HashMap.alter name (function 
                    | Some o -> mergeAttributeValues name o v |> Some
                    | None -> Some v
                )
        AMap.ofHashMap store |> AttributeMap

    let ofList (values : list<string * AttributeValue>) =
        ofSeq values

    let ofArray (values : array<string * AttributeValue>) =
        ofSeq values

    
    let union (l : AttributeMap) (r : AttributeMap) =
        AMap.unionWith mergeAttributeValues l.Store r.Store |> AttributeMap

    let add (name : string) (value : AttributeValue) (map : AttributeMap) =
        union map (single name value)

    let singleAdaptive (name : string) (value : aval<AttributeValue>) =
        value |> AVal.map (fun v -> HashMap.single name v) |> AMap.ofAVal |> AttributeMap

    let ofSeqAdaptive (values : seq<string * aval<AttributeValue>>) =
        (empty, values) ||> Seq.fold (fun map (name, value) -> 
            union map (singleAdaptive name value)
        )

    let ofListAdaptive (values : list<string * aval<AttributeValue>>) =
        ofSeqAdaptive values
        
    let ofArrayAdaptive (values : list<string * aval<AttributeValue>>) =
        ofSeqAdaptive values

    let addDynamic (name : string) (value : aval<AttributeValue>) (map : AttributeMap) =
        let entry = value |> AVal.map (fun v -> HashMap.single name v) |> AMap.ofAVal |> AttributeMap
        union map entry
        
    let singleAdaptiveOption (name : string) (value : aval<option<AttributeValue>>) =
        value 
        |> AVal.map (function Some v -> HashMap.single name v | None -> HashMap.empty) 
        |> AMap.ofAVal 
        |> AttributeMap
        
    let ofSeqAdaptiveOption (values : seq<string * aval<option<AttributeValue>>>) =
        (empty, values) ||> Seq.fold (fun map (name, value) -> 
            union map (singleAdaptiveOption name value)
        )

[<AutoOpen>]
module AttributeMapBuilder = 

    [<Sealed>]
    type AttributeMapBuilder() =
    
        member inline x.Yield((name : string, value : string)) =
            AttributeMap.single name (AttributeValue.String value)
        
        member inline x.Yield((name : string, desc : EventDescription)) =
            AttributeMap.single name (AttributeValue.Event [desc])
        
        member inline x.Yield((name : string, value : AttributeValue)) =
            AttributeMap.single name value
        
        member inline x.Yield((name : string, value : aval<string>)) =
            AttributeMap.singleAdaptive name (AVal.map AttributeValue.String value)
        
        member inline x.Yield((name : string, value : aval<option<string>>)) =
            AttributeMap.singleAdaptiveOption name (AVal.map (Option.map AttributeValue.String) value)
        
        member inline x.Yield((name : string, value : aval<EventDescription>)) =
            AttributeMap.singleAdaptive name (AVal.map (List.singleton >> AttributeValue.Event) value)
        
        member inline x.Yield((name : string, value : aval<option<EventDescription>>)) =
            AttributeMap.singleAdaptiveOption name (AVal.map (Option.map (List.singleton >> AttributeValue.Event)) value)
        
        member inline x.Yield((name : string, value : aval<AttributeValue>)) =
            AttributeMap.singleAdaptive name value

        member inline x.Yield((name : string, value : aval<option<AttributeValue>>)) =
            AttributeMap.singleAdaptiveOption name value
        
        member inline x.Yield(value : aval<option<string * string>>) =
            value 
            |> AVal.map (function Some (name, value) -> [name, AttributeValue.String value] | None -> [])
            |> AMap.ofAVal
            |> AttributeMap
        
        member inline x.Yield(value : aval<option<string * EventDescription>>) =
            value 
            |> AVal.map (function Some (name, value) -> [name, AttributeValue.Event [value]] | None -> [])
            |> AMap.ofAVal
            |> AttributeMap
           
        member inline x.Yield(value : aval<option<string * AttributeValue>>) =
            value 
            |> AVal.map (fun v -> console.error(v); v)
            |> AVal.map (function Some (name, value) -> HashMap.ofList [name, value] | None -> HashMap.empty)
            |> AMap.ofAVal
            |> AttributeMap

        member inline x.Yield(value : aval<string * string>) =
            value 
            |> AVal.map (fun (name, value) -> [name, AttributeValue.String value])
            |> AMap.ofAVal
            |> AttributeMap
        
        member inline x.Yield(value : aval<string * EventDescription>) =
            value 
            |> AVal.map (fun (name, value) -> [name, AttributeValue.Event [value]])
            |> AMap.ofAVal
            |> AttributeMap
        
        member inline x.Yield(value : aval<string * AttributeValue>) =
            value 
            |> AVal.map (fun (name, value) -> [name, value])
            |> AMap.ofAVal
            |> AttributeMap
        member inline x.Zero() =
            AttributeMap.empty

        member inline x.Delay(f : unit -> AttributeMap) = f

        member inline x.Combine(l : AttributeMap, r : unit -> AttributeMap) =
            AttributeMap.union l (r())
        
        member inline x.Run(f : unit -> AttributeMap) = f()
        
    let att = AttributeMapBuilder()

