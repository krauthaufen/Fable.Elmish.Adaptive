namespace Fable.React.Adaptive.JsHelpers

open Fable.Core
open Fable.Core.JsInterop
open Browser

/// Bindings for JS `performance` module
module Performance = 
    /// Gets the time in milliseconds since application startup.
    [<Emit("performance.now()")>]
    let now() : float = jsNative

    [<Emit("performance.mark($0)")>]
    let mark (name : string) : unit = jsNative


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
    let defineProperty (o : obj) (name : string) (prop : obj) : unit = jsNative

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
        member inline x.DefineProperty(name : string, getter : unit -> 'a, setter : 'a -> unit) =  
            defineProperty x name (createObj ["get", box getter; "set", box setter])

        /// Defines a property for the object using a getter- and an optional setter-function.
        member inline x.DefineProperty(name : string, getter : unit -> 'a) =  
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


[<AutoOpen>]
module MyPromiseBuilder =
    open Fable.Core.JS

    type MyPromiseBuilder() =
        
        member inline x.Zero() = Fable.Core.JS.Constructors.Promise.resolve ()

        member inline x.Bind(p : Promise<'a>, mapping : 'a -> Promise<'b>) =
            p.``then``(mapping) |> unbox<Promise<'b>>
            
        member inline x.Return(v : 'a) =
            Fable.Core.JS.Constructors.Promise.resolve v

        member inline x.Delay(action : unit -> Promise<'a>) =
            action
            //Promise.Create(fun s e ->
            //    action().``then``(s, e) |> ignore
            //) |> unbox<Promise<'a>>

        member inline x.Combine(l : Promise<unit>, r : unit -> Promise<'a>) =   
            l.``then``(r) |> unbox<Promise<'a>>

        member inline x.Run(action : unit -> Promise<'a>) = action()

        member inline x.For(s : seq<'a>, mapping : 'a -> Promise<unit>) =
            let rec run (mapping : 'a -> Promise<unit>) (e : System.Collections.Generic.IEnumerator<'a>) =
                if e.MoveNext() then
                    mapping(e.Current).``then``(fun () -> run mapping e) |> unbox<Promise<unit>>
                else
                    e.Dispose()
                    Fable.Core.JS.Constructors.Promise.resolve ()
            let e = s.GetEnumerator()
            run mapping e
                
    let prom = MyPromiseBuilder()


