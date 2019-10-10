module Entry


open System
open Browser
open FSharp.Data.Adaptive
open Fable.Elmish.Adaptive
open Fable.Elmish.Adaptive.Generic

module Test =
    
    let test (elements : alist<string>) =
        div [ clazz "myclass" ] [
            div [] "Header"
            div [] []

            br []
            br []

            div (att { clazz "foo"; AVal.constant (clazz "someclass")}) (
                elements |> AList.map (fun e ->
                    div [clazz "element"; click id; mousemove id] e
                )
            )

        ]




[<EntryPoint>]
let main argv =
    let c = cval 10
    let bla = Test.test (AList.ofList ["a"; "b"; "c" ])

    


    document.write (sprintf "c: %A"  (AVal.force c))
    0 // return an integer exit code
