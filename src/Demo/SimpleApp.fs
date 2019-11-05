module SimpleApp

open FSharp.Data.Adaptive
open Fable.Elmish.Adaptive
open Fable.React
open Fable.React.Props
open Fable.React.Adaptive
open Browser
open Fable.JsHelpers

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
    let headerAttributes =
        attr {
            model |> AVal.map (fun v ->
                if v < 0 then Style [Color "red"] |> Some
                elif v > 0 then Style [Color "darkgreen"] |> Some
                else None
            )
            model |> AVal.map (fun v ->
                if v < 0 then 
                    OnMouseEnter (fun _ -> emit Increment) |> Some
                else
                    None
            )

        }
    div [] [
        ah3 headerAttributes (AList.single (str "Simple Counter"))
        astr (AVal.map string model)
        br []
        button [OnClick (fun _ -> emit Increment)] [ str "+" ]
        button [OnClick (fun _ -> emit Decrement)] [ str "-" ]
    ]

let app =
    {
        init = init
        update = update
        view = view
        unpersist = Unpersist.value
    }

let run () =
    let div = document.createElement "div"
    document.body.appendChild div |> ignore
    App.run div None app |> ignore
