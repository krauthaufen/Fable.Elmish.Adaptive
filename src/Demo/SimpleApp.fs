module SimpleApp

open FSharp.Data.Adaptive
open Fable.Elmish.Adaptive
open Fable.Elmish.Adaptive.Generic
open Browser


type Message =
    | Increment 
    | Decrement

let init() =
    0
        
let update (model : int) (message : Message) =
    match message with
    | Increment -> model + 1
    | Decrement -> model - 1

let view (model : aval<int>) (emit : Message -> unit) =
    let headerAtttributes =
        att {
            model |> AVal.map (fun v ->
                if v < 0 then style "color: red" |> Some
                elif v > 0 then style "color: darkgreen" |> Some
                else None
            )
        }
    div [] [
        h3 headerAtttributes "Simple Counter"
        str (AVal.map string model)
        br []
        button [click (fun () -> emit Increment)] "+"
        button [click (fun () -> emit Decrement)] "-"
    ]

let app =
    {
        init = init
        update = update
        view = view
        unpersist = Unpersist.value
    }

let run () =
    App.run document.body None app |> ignore
