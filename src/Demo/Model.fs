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
