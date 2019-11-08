module SimpleApp

open FSharp.Data.Adaptive
open Fable.Elmish.Adaptive
open Fable.React
open Fable.React.Props
open Fable.React.Adaptive
open Browser
open Fable.JsHelpers
open Model

type Message =
    | Increment 
    | Decrement
    | AppendNumber

let init() =
    { 
        value = 0
        color = None 
        things = IndexList.ofList [ { name = "one"; value = 1 } ]    
    }
      
let getColor (v : int) =
    if v > 0 then Some "darkgreen"
    elif v < 0 then Some "red"
    else None

let update (model : Model) (message : Message) =
    match message with
    | AppendNumber ->
        { model with things = IndexList.add { name = "one"; value = model.value } model.things }
    | Increment -> 
        let value = model.value + 1
        { model with value = value; color = getColor value }
    | Decrement -> 
        let value = model.value - 1
        { model with value = value; color = getColor value }

let view (model : AdaptiveModel) (emit : Message -> unit) =
    let headerAttributes =
        attr {
            model.color |> AVal.map (function Some c -> Style [Color c] |> Some | None -> None)
            //model.value |> AVal.map (fun v ->
            //    if v < 0 then Style [Color "red"] |> Some
            //    elif v > 0 then Style [Color "darkgreen"] |> Some
            //    else None
            //)
            model.value |> AVal.map (fun v ->
                if v < 0 then 
                    OnMouseEnter (fun _ -> emit Increment) |> Some
                else
                    None
            )

        }
    div [] [
        ah3 headerAttributes (AList.single (str "Simple Counter"))
        astr (AVal.map string model.value)
        br []
        button [OnClick (fun _ -> emit Increment)] [ str "+" ]
        button [OnClick (fun _ -> emit Decrement)] [ str "-" ]
        button [OnClick (fun _ -> emit AppendNumber)] [ str "Num" ]

        aul AttributeMap.empty (
            model.things |> AList.map (fun t ->
                li [] [
                    span [Style [FontFamily "monospace"]] [ 
                        astr t.name 
                        str ": "
                        span [Style [Color "green"]] [    
                            t.value |> AVal.map string |> astr
                        ]
                    ]
                

                ]
            )
        )

    ]

let app =
    {
        init = init
        update = update
        view = view
        unpersist =
            {
                create = AdaptiveModel
                update = fun t v -> t.update v
            }
    }

let run () =
    let div = document.createElement "div"
    document.body.appendChild div |> ignore
    App.run div None app |> ignore
