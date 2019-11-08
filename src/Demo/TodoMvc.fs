(**
- title: Todo MVC
- tagline: The famous todo mvc ported from elm-todomvc
*)

module TodoMvcApp

open Fable.Core
open Fable.Import
open Elmish
open TodoMvc
open FSharp.Data.Adaptive
open Fable.React.Adaptive
open Fable.Elmish.Adaptive

module AList =
    let filterA (p : 'a -> aval<bool>) (l : alist<'a>) =
        l |> AList.collect ( fun a -> 
            p a |> AVal.map ( function true -> IndexList.single a | false -> IndexList.empty ) |> AList.ofAVal
        )

    let forallA (p : 'a -> aval<bool>) (l : alist<'a>) =
        l |> filterA ( p >> AVal.map not ) |> AList.toAVal |> AVal.map ( fun l -> l.Count = 0 )
        
    let count (l : alist<'a>) =
        l |> AList.toAVal |> AVal.map IndexList.count
    
    let isEmpty (l : alist<'a>) =
        l |> AList.toAVal |> AVal.map IndexList.isEmpty

let [<Literal>] ESC_KEY = 27.
let [<Literal>] ENTER_KEY = 13.
let [<Literal>] ALL_TODOS = "all"
let [<Literal>] ACTIVE_TODOS = "active"
let [<Literal>] COMPLETED_TODOS = "completed"



let emptyModel =
   { entries = IndexList.empty
     visibility = ALL_TODOS
     field = ""
     uid = 0 }

let newEntry desc id =
 { description = desc
   completed = false
   editing = false }


let init = function
 | Some savedModel -> savedModel
 | _ -> emptyModel


// UPDATE


(** Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
*)
type Msg =
   | Failure of string
   | UpdateField of string
   | EditingEntry of Index*bool
   | UpdateEntry of Index*string
   | Add
   | Delete of Index
   | DeleteComplete
   | Check of Index*bool
   | CheckAll of bool
   | ChangeVisibility of string



// How we update our Model on a given Msg?
let update (msg:Msg) (model:Model) : Model =
   match msg with
   | Failure err ->
       Fable.Core.JS.console.error(err)
       model

   | Add ->
       let xs = if System.String.IsNullOrEmpty model.field then
                   model.entries
                else
                   IndexList.prepend (newEntry model.field model.uid) model.entries
       { model with
           uid = model.uid + 1
           field = ""
           entries = xs }

   | UpdateField str ->
     { model with field = str }

   | EditingEntry (id,isEditing) ->
       let updateEntry t =
         { t with editing = isEditing }
       { model with entries = IndexList.update id updateEntry model.entries }

   | UpdateEntry (id,task) ->
       let updateEntry t =
         { t with description = task }
       { model with entries = IndexList.update id updateEntry model.entries }

   | Delete id ->
       { model with entries = IndexList.remove id model.entries }

   | DeleteComplete ->
       { model with entries = IndexList.filter (fun t -> not t.completed) model.entries }

   | Check (id,isCompleted) ->
       let updateEntry t =
         { t with completed = isCompleted }
       { model with entries = IndexList.update id updateEntry model.entries }

   | CheckAll isCompleted ->
       let updateEntry t = { t with completed = isCompleted }
       { model with entries = IndexList.map updateEntry model.entries }

   | ChangeVisibility visibility ->
       { model with visibility = visibility }

//// Local storage interface
//module S =
//   let private STORAGE_KEY = "elmish-react-todomvc"
//   let private decoder = Thoth.Json.Decode.Auto.generateDecoder<Model>()
//   let load (): Model option =
//       Browser.WebStorage.localStorage.getItem(STORAGE_KEY)
//       |> unbox
//       |> Core.Option.bind (Thoth.Json.Decode.fromString decoder >> function | Ok r -> Some r | _ -> None)

//   let save (model: Model) =
//       Browser.WebStorage.localStorage.setItem(STORAGE_KEY, Thoth.Json.Encode.Auto.toString(1,model))


//let setStorage (model:Model) : Cmd<Msg> =
//   Cmd.OfFunc.attempt S.save model (string >> Failure)

//let updateWithStorage (msg:Msg) (model:Model) =
// match msg with
// // If the Msg is Failure we know the model hasn't changed
// | Failure _ -> model, []
// | _ ->
//   let (newModel, cmds) = update msg model
//   newModel, Cmd.batch [ setStorage newModel; cmds ]

// rendering views with React
open Fable.React.Props
open Fable.React
open Fable.Core.JsInterop
open Elmish.React

let internal onEnter clear msg dispatch =
   function
   | (ev:Browser.Types.KeyboardEvent) when ev.keyCode = ENTER_KEY ->
       if clear then ev.target?value <- ""
       dispatch msg
   | _ -> ()
   |> OnKeyDown

let viewInput (model:aval<string>) dispatch =
   header [ ClassName "header" ] [
       h1 [] [ str "todos" ]
       ainput (
           attr {
               ClassName "new-todo"
               Placeholder "What needs to be done?"
               AVal.map valueOrDefault model
               onEnter true Add dispatch
               OnInput (fun ev -> !!ev.target?value |> UpdateField |> dispatch)
               AutoFocus true
           }
       )
   ]

let internal classList classes =
   classes
   |> List.fold (fun complete -> function | (name,true) -> complete + " " + name | _ -> complete) ""
   |> ClassName

let viewEntry (id : Index) (todo : AdaptiveEntry) dispatch =
 ali
   (attr { 
        todo.completed |> AVal.map ( fun t -> if t then Some (Class "completed") else None )
        todo.editing |> AVal.map ( fun t -> if t then Some (Class "editing") else None )
    })
   (AList.ofList [ 
     div
       [ ClassName "view" ]
       [ ainput (
            attr { 
                 ClassName "toggle"
                 Type "checkbox"
                 AVal.map Checked todo.completed
                 OnChange (fun _ -> Check (id,(not (AVal.force todo.completed))) |> dispatch)
           }
         )
         label
           [ OnDoubleClick (fun _ -> EditingEntry (id,true) |> dispatch) ]
           [ astr todo.description ]
         button
           [ ClassName "destroy"
             OnClick (fun _-> Delete id |> dispatch) ]
           []
       ]
     ainput
       (attr { 
         ClassName "edit"
         AVal.map (box >> Value) todo.description
         Name "title"
         OnInput (fun ev -> UpdateEntry (id, !!ev.target?value) |> dispatch)
         OnBlur (fun _ -> EditingEntry (id,false) |> dispatch)
         onEnter false (EditingEntry (id,false)) dispatch })
   ])

let viewEntries (visibility : aval<string>) (model : AdaptiveModel) dispatch =
   let entries = model.entries
   let isVisible (todo : AdaptiveEntry) =
       visibility |> AVal.bind ( fun visibility -> 
           match visibility with
           | COMPLETED_TODOS -> todo.completed
           | ACTIVE_TODOS -> AVal.map not todo.completed
           | _ -> AVal.constant true
        )

   let allCompleted =
       model.AllCompleted

   let cssVisibility =
       AList.toAVal entries |> AVal.map ( fun l -> if l |> IndexList.isEmpty then "hidden" else "visible" )

   section
     [ ClassName "main"
       Style [ Visibility cssVisibility ]]
     [ ainput ( 
         attr { 
               ClassName "toggle-all"
               Type "checkbox"
               Name "toggle"
               AVal.map Checked allCompleted
               OnChange (fun _ -> CheckAll (not (AVal.force allCompleted)) |> dispatch)}
           )
       label
         [ HtmlFor "toggle-all" ]
         [ str "Mark all as complete" ]
       aul
         (AttributeMap.ofList [ ClassName "todo-list" ])
         (entries
          |> AList.filterA isVisible
          |> AList.mapi (fun i e -> viewEntry i e dispatch)) ]

// VIEW CONTROLS AND FOOTER
let visibilitySwap uri visibility (actualVisibility : aval<string>) dispatch =
 li
   [ OnClick (fun _ -> ChangeVisibility visibility |> dispatch) ]
   [ aa (
        attr { 
            Href uri
            AVal.map (fun a -> if a = visibility then Some (Class "selected") else None) actualVisibility
            //classList ["selected", visibility = actualVisibility] 
        }
     ) (AList.ofList [ str visibility ]) 
   ]

let viewControlsFilters (visibility : aval<string>) dispatch =
 ul
   [ ClassName "filters" ]
   [ visibilitySwap "#/" ALL_TODOS visibility dispatch
     str " "
     visibilitySwap "#/active" ACTIVE_TODOS visibility dispatch
     str " "
     visibilitySwap "#/completed" COMPLETED_TODOS visibility dispatch ]

let viewControlsCount (entriesLeft : aval<int>)=
 let item e =
     if e = 1 then " item" else " items"

 span
     [ ClassName "todo-count" ]
     [ strong [] [ astr (AVal.map string entriesLeft) ]
       astr (entriesLeft |> AVal.map ( fun l -> item l + " left") ) ]

let viewControlsClear (entriesCompleted : aval<int>) dispatch =
 abutton
   (attr { 
        ClassName "clear-completed"
        AVal.map ( fun e -> Hidden (e = 0) ) entriesCompleted
        OnClick (fun _ -> DeleteComplete |> dispatch)
    })
    (AList.ofList [ astr (AVal.map ( fun e -> "Clear completed (" + (string e) + ")") entriesCompleted) ] )

let viewControls (visibility : aval<string>) (entries : alist<AdaptiveEntry>) dispatch =
 let entriesCompleted =
     entries
     |> AList.filterA (fun t -> t.completed)
     |> AList.count

 let entriesLeft =
    AVal.map2 (-) (AList.count entries) entriesCompleted

 afooter
     (attr { 
        ClassName "footer"
        AVal.map Hidden (AList.isEmpty entries) 
     })
     (AList.ofList [ 
        viewControlsCount entriesLeft 
        viewControlsFilters visibility dispatch
        viewControlsClear entriesCompleted dispatch ])

let infoFooter =
 footer [ ClassName "info" ]
   [ p []
       [ str "Double-click to edit a todo" ]
     p []
       [ str "Ported from Elm by "
         a [ Href "https://github.com/et1975" ] [ str "Eugene Tolmachev" ]]
     p []
       [ str "Part of "
         a [ Href "http://todomvc.com" ] [ str "TodoMVC" ]]
   ]

let view (model : AdaptiveModel) dispatch =
 div
   [ ClassName "todomvc-wrapper"]
   [ section
       [ ClassName "todoapp" ]
       [ viewInput model.field dispatch
         viewEntries model.visibility model dispatch
         viewControls model.visibility model.entries dispatch ]
     infoFooter ]

let initMe () =
    init None

let updateMe (model : Model) (msg : Msg) : Model = update msg model
   

let app =
    {
        init = initMe
        update = updateMe
        view = view
        unpersist =
            {
                create = AdaptiveModel
                update = fun t v -> t.update v
            }
    }

open global.Browser
let runMe () =
    let div = document.createElement "div"
    let link = document.createElement "link"
    link.setAttribute("rel","stylesheet")
    link.setAttribute("type","text/css")
    link.setAttribute("href","index.css")

    document.body.appendChild div |> ignore
    document.head.appendChild link |> ignore
     
    App.run div (Some 5) app |> ignore