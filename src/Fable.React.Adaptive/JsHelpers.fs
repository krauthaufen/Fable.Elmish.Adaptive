namespace Fable.JsHelpers

open Fable.Core
open Fable.Core.JsInterop
open Browser

/// Bindings for JS `performance` module
module Performance = 
    /// Gets the time in milliseconds since application startup.
    [<Emit("performance.now()")>]
    let now() : float = jsNative

/// JS timeout handle.
[<AllowNullLiteral>]
type Timeout = class end

/// Operators for creating/deleting timeouts.
module Timeout = 
    /// Enqueues an action that will be executed in (at least) the given number of milliseconds.
    [<Emit("setTimeout($1, $0)")>]
    let set (ms : int) (action : unit -> unit) : Timeout = jsNative

    /// Clears the given timeout.
    [<Emit("clearTimeout($0)")>]
    let clear (t : Timeout) : unit = jsNative


/// Operators for working with JS types.
module JsType = 
    /// Is the given object a function?
    [<Emit("typeof $0 === \"function\"")>]
    let isFunction (o : obj) : bool = jsNative
    
    /// Is the given object a JS object? (as opposed to string/number/function/etc.)
    [<Emit("typeof $0 === \"object\"")>]
    let isObject (o : obj) : bool = jsNative
    
    /// Is the given object a string?
    [<Emit("typeof $0 === \"string\"")>]
    let isString (o : obj) : bool = jsNative

    /// Is the given object a number?
    [<Emit("typeof $0 === \"number\"")>]
    let isNumber (o : obj) : bool = jsNative
   
/// JS related Extensions for System.Object.
[<AutoOpen>]
module JsHelperExtensions = 

    /// defineProperty wrapper.
    [<Emit("Object.defineProperty($0, $1, $2)")>]
    let private defineProperty (o : obj) (name : string) (prop : obj) : unit = jsNative

    type System.Object with
        /// Assigns all props from the given objects to this one.
        [<Emit("Object.assign($0, $1...)")>]
        member x.Assign([<System.ParamArray>] others : obj[]) : unit = jsNative
            
        /// Deletes a specific property from the object.        
        [<Emit("delete $0[$1]")>]
        member x.Delete(key : string) : unit = jsNative

        /// Gets the keys for all props contained in the given object.
        [<Emit("Object.keys($0)")>]
        static member GetKeys (o : obj) : seq<string> = jsNative

        /// Gets the keys for all props contained in the object.
        member inline x.Keys = System.Object.GetKeys x

        /// Defines a property for the object using a getter- and an optional setter-function.
        member x.DefineProperty(name : string, getter : unit -> 'a, ?setter : 'a -> unit) =  
            match setter with
            | Some setter ->
                defineProperty x name (createObj ["get", box getter; "set", box setter])
            | None ->
                defineProperty x name (createObj ["get", box getter])
                
    /// All function arguments for the currently executing function.
    [<Emit("arguments")>]
    let arguments : obj[] = jsNative

    type Fable.Core.JsInterop.JsFunc with
        /// All function arguments for the currently executing function.
        static member inline Arguments : obj[] = arguments




module Log =
    let inline line fmt =
        Printf.kprintf (fun str -> console.log(str)) fmt

    let inline warn fmt =
        Printf.kprintf (fun str -> console.warn(str)) fmt

    let inline error fmt =
        Printf.kprintf (fun str -> console.error(str)) fmt

    let inline start fmt =
        Printf.kprintf (fun str -> console.group(str)) fmt
        
    let inline stop() =
        console.groupEnd()