module CollectionsJS

open System
open Fable.Core
open Fable.Core.JsInterop

[<AllowNullLiteral>]
type SortedSetNode<'T> =
    abstract value : 'T
    abstract length : int
    abstract index : int
    abstract left : SortedSetNode<'T>
    abstract right : SortedSetNode<'T>

type SortedSet<'T> =
    [<Emit("$0.push($1...)")>]
    abstract Add : [<ParamArray; ParamList>] values : 'T[] -> unit
     
    [<Emit("$0.remove($1)")>]
    abstract Remove : value : 'T -> bool

    [<Emit("$0.has($1)")>]
    abstract Contains : 'T -> bool

    [<Emit("$0.findGreatestLessThan($1)")>]
    abstract TryFindLeft : 'T -> SortedSetNode<'T>
    
    [<Emit("$0.findLeastGreaterThan($1)")>]
    abstract TryFindRight : 'T -> SortedSetNode<'T>
    
    [<Emit("$0.find($1)")>]
    abstract TryFind : 'T -> SortedSetNode<'T>
    
    [<Emit("$0.length")>]
    abstract Count : int

[<Import("SortedSet", "sorted-set-collections/sorted-set")>]
type SortedSetConstructor =
    [<EmitConstructor>]
    abstract Create<'T> : ?values : seq<'T> * ?equals : System.Func<'T, 'T, bool> * ?compare : System.Func<'T, 'T, int> * ?getDefault : System.Func<'T> -> SortedSet<'T>

        
[<Import("SortedSet", "sorted-set-collections/sorted-set")>]
let internal sortedSet : SortedSetConstructor = import "SortedSet" "sorted-set-collections/sorted-set"


type private SortedMapEntry<'K, 'V> =
    {
        key : 'K
        mutable value : 'V
    }

type SortedMap<'K, 'V> = private { store : SortedSet<SortedMapEntry<'K, 'V>> }

module SortedMap =

    let create<'K, 'V when 'K : comparison>() =
        
        let equals l r = Unchecked.equals l.key r.key
        let compare l r = compare l.key r.key

        { store = sortedSet.Create<SortedMapEntry<'K, 'V>>([], System.Func<_,_,_>(equals), System.Func<_,_,_>(compare)) }

    let add (key : 'K) (value : 'V) (set : SortedMap<'K, 'V>) =
        let self = { key = key; value = value }
        let node = set.store.TryFind self
        if isNull node then set.store.Add self
        else node.value.value <- value

    let tryFind (key : 'K) (set : SortedMap<'K, 'V>) =
        let node = set.store.TryFind { key = key; value = Unchecked.defaultof<_> }
        if isNull node then None
        else Some node.value.value

    let neigbours (key : 'K) (set : SortedMap<'K, 'V>) =
        let foo = { key = key; value = Unchecked.defaultof<_> }
        let left = set.store.TryFindLeft foo
        let self = set.store.TryFind foo
        let right = set.store.TryFindRight foo

        let l = if isNull left then None else Some left.value.value
        let s = if isNull self then None else Some self.value.value
        let r = if isNull right then None else Some right.value.value
        (l, s, r)

    let tryRemove (key : 'K) (set : SortedMap<'K, 'V>) =
        let foo = { key = key; value = Unchecked.defaultof<_> }
        let self = set.store.TryFind foo
        if isNull self then 
            None
        else 
            set.store.Remove self.value |> ignore
            Some self.value.value

    let alterNeigbours (key : 'K) (update : Option<'V> -> Option<'V>) (set : SortedMap<'K, 'V>) =
        
        let foo = { key = key; value = Unchecked.defaultof<_> }
        //let left = set.store.TryFindLeft foo
        let self = set.store.TryFind foo
        let right = set.store.TryFindRight foo

        //let l = if isNull left then None else Some left.value.value
        //let s = if isNull self then None else Some self.value.value
        let r = if isNull right then None else Some right.value.value

        match update r with
        | Some res ->
            if isNull self then
                foo.value <- res
                set.store.Add foo
            else
                self.value.value <- res
        | None ->
            if not (isNull self) then
                set.store.Remove foo |> ignore

    let count (m : SortedMap<'K, 'V>) = m.store.Count

    let alterNeigboursPromise (key : 'K) (update : Option<'V> -> Option<'V> -> Option<'V> -> JS.Promise<Option<'V>>) (set : SortedMap<'K, 'V>) =
        
        let foo = { key = key; value = Unchecked.defaultof<_> }
        let left = set.store.TryFindLeft foo
        let self = set.store.TryFind foo
        let right = set.store.TryFindRight foo

        let l = if isNull left then None else Some left.value.value
        let s = if isNull self then None else Some self.value.value
        let r = if isNull right then None else Some right.value.value

        (update l s r).``then``(fun res ->
            match res with
            | Some res ->
                if isNull self then
                    foo.value <- res
                    set.store.Add foo
                else
                    self.value.value <- res
            | None ->
                if not (isNull self) then
                    set.store.Remove foo |> ignore
        )

