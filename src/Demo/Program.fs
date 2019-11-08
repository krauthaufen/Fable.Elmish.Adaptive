module Entry

open Browser


[<EntryPoint>] 
let main argv =
    document.addEventListener("readystatechange", fun _ ->
        if document.readyState = "complete" then
            //AdaptiveListDemo.run()
            SimpleApp.run()
    )
    0
