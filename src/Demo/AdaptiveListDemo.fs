module AdaptiveListDemo 

open System
open Browser
open FSharp.Data.Adaptive
open React.Adaptive
open Fable.React
open Fable.React.Props

let rand = Random()

let randomString() =
    let c = rand.Next(26) + int 'A' |> char
    System.String [|c|]

let run() =

    let initial = IndexList.ofList ["a"; "b"; "c"]
    let list = clist initial

    let append _ =
        transact (fun () ->
            list.Append (randomString()) |> ignore
        )

    let prepend _ =
        transact (fun () ->
            list.Prepend (randomString()) |> ignore
        )

    let clear _ =
        transact (fun () ->
            list.Value <- initial
        )

    let ui = 
        div [ ] [
            button [OnClick prepend] [ str "Prepend"]
            button [OnClick append] [ str "Append"]
            button [OnClick clear] [ str "Clear"]
                
            aol (
                list |> AList.map (fun text ->
                    li [] [str text]
                )
            )
        ]

    ReactDom.render(ui, document.body)
