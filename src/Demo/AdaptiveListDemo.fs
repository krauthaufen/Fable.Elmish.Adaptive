module AdaptiveListDemo 

open System
open Browser
open FSharp.Data.Adaptive
open React.Adaptive
open Fable.React
open Fable.React.Props

let random = Random()

// simple utility for creating random strings
let randomString() =
    let randomChar() = 
        if random.Next 2 = 0 then random.Next(26) + int 'A' |> char
        else random.Next(26) + int 'a' |> char

    Array.init (random.Next 5 + 2) (fun _ -> randomChar())
    |> System.String

let run() =

    // create a changeable list with our initial content
    let initial = IndexList.ofList ["a"; "b"; "c"]
    let list = clist initial

    // callback inserting a new string at a random position in the list
    let insert _ =
        transact (fun () ->
            let id = random.Next list.Count
            list.InsertAt(id, randomString()) |> ignore
        )

    // callback appending a new string to the list
    let append _ =
        transact (fun () ->
            list.Append (randomString()) |> ignore
        )
        
    // callback prepending a new string to the list
    let prepend _ =
        transact (fun () ->
            list.Prepend (randomString()) |> ignore
        )
        
    // callback resetting the list to the initial content
    let clear _ =
        transact (fun () ->
            list.Value <- initial
        )

    let ui = 
        div [ ] [
            // some buttons for changing the list
            button [OnClick prepend] [ str "Prepend"]
            button [OnClick insert] [ str "Insert"]
            button [OnClick append] [ str "Append"]
            button [OnClick clear] [ str "Reset"]
              
            // we simply use the `aul` component here (short for adaptive-ul)
            // that uses the alist-changes to update the dom accordingly without
            // using react's reconciler for updating the list.
            // NOTE that the current implementation cannot handle any HTML attributes
            //      but we plan to support these too as the library evolves...
            aul (
                list |> AList.map (fun text ->
                    // for illustration purposes we log various actions here.
                    console.log("create node", text)

                    // DebugComponents.withLogging just adds proper logging to
                    // `componentDidMount`, `componentWillMount`, etc. for validation purposes.
                    li [Style [FontFamily "monospace"]] [
                        DebugComponents.withLogging (
                            div [Style [Color "darkred"]] [str text] 
                        )
                    ]
                )
            )
        ]

    ReactDom.render(ui, document.body)
