//5c5f6775-6fed-a0fb-32e9-8c2ac1d7e414
//9d23185a-806b-14fd-f171-7f2de4bcb487
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
namespace rec Model

open System
open FSharp.Data.Adaptive
open Adaptify
open Model
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveNamed<'a, '_prima, '_aa>(value : Named<'a>, _primainit : 'a -> System.Object, _primaupdate : System.Object -> 'a -> System.Object, _primaview : System.Object -> '_prima, _ainit : 'a -> System.Object, _aupdate : System.Object -> 'a -> System.Object, _aview : System.Object -> '_aa) =
    let _value_ = _ainit value.value
    let _name_ = FSharp.Data.Adaptive.cval(value.name)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Named<'a>, _primainit : 'a -> System.Object, _primaupdate : System.Object -> 'a -> System.Object, _primaview : System.Object -> '_prima, _ainit : 'a -> System.Object, _aupdate : System.Object -> 'a -> System.Object, _aview : System.Object -> '_aa) = AdaptiveNamed<'a, '_prima, '_aa>(value, _primainit, _primaupdate, _primaview, _ainit, _aupdate, _aview)
    member __.Update(value : Named<'a>) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Named<'a>>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            ignore (_aupdate _value_ value.value)
            _name_.Value <- value.name
    member __.Current = __adaptive
    member __.value = _aview _value_
    member __.name = _name_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveModel(value : Model) =
    let _value_ = FSharp.Data.Adaptive.cval(value.value)
    let _color_ = FSharp.Data.Adaptive.cval(value.color)
    let _things_ =
        let inline __arg1 (v : Named<Microsoft.FSharp.Core.int>) =
            let inline __arg5 (o : System.Object) (v : Microsoft.FSharp.Core.int) =
                (unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o).Value <- v
                o
            AdaptiveNamed<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>>(v, (fun (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) (v : Microsoft.FSharp.Core.int) -> v :> System.Object), (fun (o : System.Object) -> unbox<Microsoft.FSharp.Core.int> o), (fun (v : Microsoft.FSharp.Core.int) -> FSharp.Data.Adaptive.cval(v) :> System.Object), __arg5, (fun (o : System.Object) -> unbox<FSharp.Data.Adaptive.cval<Microsoft.FSharp.Core.int>> o :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>))
        let inline __arg2 (m : AdaptiveNamed<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>>) (v : Named<Microsoft.FSharp.Core.int>) =
            m.Update(v)
            m
        FSharp.Data.Traceable.ChangeableModelList(value.things, __arg1, __arg2, (fun (m : AdaptiveNamed<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>>) -> m))
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Model) = AdaptiveModel(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Model) -> AdaptiveModel(value)) (fun (adaptive : AdaptiveModel) (value : Model) -> adaptive.Update(value))
    member __.Update(value : Model) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Model>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _value_.Value <- value.value
            _color_.Value <- value.color
            _things_.Update(value.things)
    member __.Current = __adaptive
    member __.value = _value_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    member __.color = _color_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.Option<Microsoft.FSharp.Core.string>>
    member __.things = _things_ :> FSharp.Data.Adaptive.alist<AdaptiveNamed<Microsoft.FSharp.Core.int, Microsoft.FSharp.Core.int, FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>>>


namespace rec TodoMvc

open System
open FSharp.Data.Adaptive
open Adaptify
open TodoMvc
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveEntry(value : Entry) =
    let _description_ = FSharp.Data.Adaptive.cval(value.description)
    let _completed_ = FSharp.Data.Adaptive.cval(value.completed)
    let _editing_ = FSharp.Data.Adaptive.cval(value.editing)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Entry) = AdaptiveEntry(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Entry) -> AdaptiveEntry(value)) (fun (adaptive : AdaptiveEntry) (value : Entry) -> adaptive.Update(value))
    member __.Update(value : Entry) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Entry>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _description_.Value <- value.description
            _completed_.Value <- value.completed
            _editing_.Value <- value.editing
    member __.Current = __adaptive
    member __.description = _description_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
    member __.completed = _completed_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>
    member __.editing = _editing_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveModel(value : Model) =
    let _entries_ =
        let inline __arg2 (m : AdaptiveEntry) (v : Entry) =
            m.Update(v)
            m
        FSharp.Data.Traceable.ChangeableModelList(value.entries, (fun (v : Entry) -> AdaptiveEntry(v)), __arg2, (fun (m : AdaptiveEntry) -> m))
    let _field_ = FSharp.Data.Adaptive.cval(value.field)
    let _visibility_ = FSharp.Data.Adaptive.cval(value.visibility)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Model) = AdaptiveModel(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Model) -> AdaptiveModel(value)) (fun (adaptive : AdaptiveModel) (value : Model) -> adaptive.Update(value))
    member __.Update(value : Model) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Model>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _entries_.Update(value.entries)
            _field_.Value <- value.field
            _visibility_.Value <- value.visibility
    member __.Current = __adaptive
    member __.entries = _entries_ :> FSharp.Data.Adaptive.alist<AdaptiveEntry>
    member __.field = _field_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
    member __.visibility = _visibility_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>

