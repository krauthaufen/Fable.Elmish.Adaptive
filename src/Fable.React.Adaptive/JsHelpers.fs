namespace Fable.JsHelpers

open Fable.Core
open Fable.Core.JsInterop
open Browser

module Performance = 
    [<Emit("performance.now()")>]
    let now() : float = jsNative

[<AllowNullLiteral>]
type Timeout = class end

module Timeout = 
    [<Emit("setTimeout($1, $0)")>]
    let set (delay : int) (action : unit -> unit) : Timeout = jsNative

    [<Emit("clearTimeout($0)")>]
    let clear (t : Timeout) : unit = jsNative

module JsType = 
    [<Emit("typeof $0 === \"function\"")>]
    let isFunction (o : obj) : bool = jsNative
    
    [<Emit("typeof $0 === \"object\"")>]
    let isObject (o : obj) : bool = jsNative
    
    [<Emit("typeof $0 === \"string\"")>]
    let isString (o : obj) : bool = jsNative

    [<Emit("typeof $0 === \"number\"")>]
    let isNumber (o : obj) : bool = jsNative
   
[<AutoOpen>]
module JsHelperExtensions = 

    [<Emit("Object.defineProperty($0, $1, $2)")>]
    let private defineProperty (o : obj) (name : string) (prop : obj) : unit = jsNative

    type System.Object with
        [<Emit("Object.assign($0, $1...)")>]
        member x.Assign([<System.ParamArray>] others : obj[]) : unit = jsNative
            
        [<Emit("delete $0[$1]")>]
        member x.Delete(key : string) : unit = jsNative

        [<Emit("Object.keys($0)")>]
        static member GetKeys (o : obj) : seq<string> = jsNative

        member inline x.Keys = System.Object.GetKeys x

        member x.DefineProperty(name : string, getter : unit -> 'a, ?setter : 'a -> unit) =  
            match setter with
            | Some setter ->
                defineProperty x name (createObj ["get", box getter; "set", box setter])
            | None ->
                defineProperty x name (createObj ["get", box getter])
                

    [<Emit("arguments")>]
    let arguments : obj[] = jsNative

    type Fable.Core.JsInterop.JsFunc with
        static member inline Arguments : obj[] = arguments




module Log =
    let inline line fmt =
        Printf.kprintf (fun str -> console.log(str)) fmt

    let inline warn fmt =
        Printf.kprintf (fun str -> console.warn(str)) fmt

    let inline error fmt =
        Printf.kprintf (fun str -> console.error(str)) fmt

    let inline start fmt =
        Printf.kprintf (fun str -> console.group str) fmt
        
    let inline stop() =
        console.groupEnd()