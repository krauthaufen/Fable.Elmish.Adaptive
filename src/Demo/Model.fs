namespace Model

open FSharp.Data.Adaptive
open Adaptify
 
[<ModelType>]
type Named<'a> =
    {
        value   : 'a
        name    : string
    }

[<ModelType>]
type Model =
    {
        value : int
        color : Option<string>
        things : IndexList<Named<int>>
    }

namespace TodoMvc

open FSharp.Data.Adaptive
open Adaptify

// MODEL
[<ModelType>]
type Entry =
   { description : string
     completed : bool
     editing : bool }

// The full application state of our todo app.
[<ModelType>]
type Model = 
   { entries : IndexList<Entry>
     field : string
     visibility : string }