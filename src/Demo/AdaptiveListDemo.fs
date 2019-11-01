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
        
    // callback changing an element at a random position in the list
    let change _ =
        transact (fun () ->
            let id = list.Value.TryGetIndex (random.Next list.Count) |> Option.get
            list.[id] <- randomString()
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

    let tag = cval "ul"
    
        
    // callback resetting the list to the initial content
    let changeType _ =
        transact (fun () ->
            match tag.Value with
            | "ul" -> tag.Value <- "ol"
            | _ -> tag.Value <- "ul"
        )

    

    let ui = 
        div [ ] [ 
            // some buttons for changing the list
            button [OnClick prepend] [ str "Prepend"]
            button [OnClick insert] [ str "Insert"]
            button [OnClick append] [ str "Append"]
            button [OnClick change] [ str "Change"]
            button [OnClick clear] [ str "Reset"]
            button [OnClick changeType] [ str "Change Type"]
              


            let children =
                list |> AList.map (fun text ->
                    // for illustration purposes we log various actions here.
                    console.log("create: ", text)

                    // DebugComponents.withLogging just adds proper logging to
                    // `componentDidMount`, `componentWillMount`, etc. for validation purposes.
                    li [Style [FontFamily "monospace"]] [
                        div [Style [Color "darkred"]] [str text]
                        |> DebugComponents.withLogging text
                    ]
                )

            // we use the generic adaptiveNode here that allows changing the tag
            // and internally handles alist-changes to update the dom accordingly without
            // using react's reconciler.
            // NOTE that the current implementation cannot handle any HTML attributes
            //      but we plan to support these too as the library evolves...
            let changeable = 
                FunctionComponent.Of (fun () ->
                    let tag = Hooks.useAdaptive tag
                    adaptiveNode tag children
                )

            changeable ()


            // here's a simpler variant not using the changeable tag

            //aol (
            //    list |> AList.map (fun text ->
            //        // for illustration purposes we log various actions here.
            //        console.log("create: ", text)

            //        // DebugComponents.withLogging just adds proper logging to
            //        // `componentDidMount`, `componentWillMount`, etc. for validation purposes.
            //        li [Style [FontFamily "monospace"]] [
            //            div [Style [Color "darkred"]] [str text]
            //            |> DebugComponents.withLogging text
            //        ]
            //    )
            //)
        ]
       
    let root = document.createElement "div"
    document.body.appendChild root |> ignore
    ReactDom.render(ui, root)
