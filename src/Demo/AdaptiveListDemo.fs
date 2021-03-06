﻿module AdaptiveListDemo 

open System
open Browser
open FSharp.Data.Adaptive
open Fable.React.Adaptive
open Fable.React
open Fable.React.Props

let random = Random()

// simple utility for creating random strings.
let randomString() =
    let randomChar() = 
        if random.Next 2 = 0 then random.Next(26) + int 'A' |> char
        else random.Next(26) + int 'a' |> char

    Array.init (random.Next 5 + 2) (fun _ -> randomChar())
    |> System.String

let run() =

    // create a changeable list with our initial content.
    let initial = IndexList.ofList ["a"; "b"; "c"]
    let list = clist initial
    let size = cval None

    // callback inserting a new string at a random position in the list.
    let insert _ =
        transact (fun () ->
            let id = random.Next list.Count
            list.InsertAt(id, randomString()) |> ignore
        )
        
    // callback changing an element at a random position in the list.
    let change _ =
        transact (fun () ->
            // There is a `clist` bug (already resolved but not packaged yet) in
            // list.[random.Next list.Count] <- randomString()

            // workaround:
            let id = list.Value.TryGetIndex (random.Next list.Count) |> Option.get
            list.[id] <- randomString()
        )

    // callback appending a new string to the list.
    let append _ =
        transact (fun () ->
            list.Append (randomString()) |> ignore
        )
        
    // callback prepending a new string to the list.
    let prepend _ =
        transact (fun () ->
            list.Prepend (randomString()) |> ignore
        )
        
    // callback resetting the list to the initial content.
    let clear _ =
        transact (fun () ->
            list.Value <- initial
            size.Value <- None
        )


    let attributes =
        attr {
            size |> AVal.map (function 
                | Some s -> Some (Style [ FontSize (sprintf "%fpx" s) ]) 
                | None -> None
            )
            Style [ FontFamily "monospace" ]
        }

    let increaseFontSize _ =
        transact (fun () ->
            match size.Value with
            | Some s -> size.Value <- Some (1.2 * s)
            | None -> size.Value <- Some 20.0
        )
    

    let ui = 
        div [ ] [ 
            // some buttons for changing the list
            button [OnClick prepend] [ str "Prepend"]
            button [OnClick insert] [ str "Insert"]
            button [OnClick append] [ str "Append"]
            button [OnClick change] [ str "Change"]
            button [OnClick clear] [ str "Reset"]
            button [OnClick increaseFontSize] [ str "Increase font size"]

            // here's a simpler variant not using the changeable tag
            aol attributes (
                list |> AList.map (fun text ->
                    // withLogging just adds some logging to the given Element hooking
                    // `componentDidMount`, `componentWillMount`, etc. for validation purposes.
                    li [Style [FontFamily "monospace"]] [
                        div [Style [Color "darkred"]] [str text]
                        |> withLogging text
                    ]
                )
            )

            div [] [
                list 
                |> AList.toAVal 
                |> AVal.map (fun l -> sprintf "%d elements" l.Count)
                |> astr
            ]
        ]
       
    let root = document.createElement "div"
    document.body.appendChild root |> ignore
    ReactDom.render(ui, root)
