namespace Fable.React.Adaptive

open Browser
open Fable.React
open Fable.React.ReactiveComponents
open Fable.JsHelpers

type internal LogComponent(value : State<string * ReactElement>) =
    inherit Component<State<string * ReactElement>, State<string * ReactElement>>(value)
    //static do ComponentHelpers.setDisplayName<LogComponent,_,_> "Log"
    do base.setInitState { value = value.value }

    override x.componentWillUnmount() =
        let (name, _) = x.state.value
        console.log(name, "will unmount")

    override x.componentDidMount() =
        let (name, _) = x.state.value
        console.log(name, "did mount")

    override x.componentDidUpdate(_, _) =
        let (name, _) = x.state.value
        if x.state.value <> x.props.value then
            x.setState(fun _ _ -> { value = x.props.value })
        console.log(name, "did update")
            

    override x.componentDidCatch(e, info) =
        let (name, _) = x.state.value
        console.log(name, "did catch", e, info)
            
    override x.shouldComponentUpdate(np, ns) =
        let (name, _) = x.state.value
        console.log(name, "should update")
        true

    override x.render() =
        let (name, content) = x.state.value
        console.log(name, "render")
        content
            

module LogComponent =
    let create (name : string) (content : ReactElement) = 
        ofType<LogComponent, _, _> { value = (name, content) } []

module AdaptiveComponents =
    open Fable.Core.JsInterop
    let mutable rendersPending = 0
    let mutable callbacks = []
    let mutable times = obj()
    
    let mutable suspended : list<string> = []
    let mutable activeGroup = None
    let mutable activeStart = 0.0

    let inline addTime (group : string) (dt : float) =
        if times?(group) then times?(group) <- times?(group) + dt
        else times?(group) <- dt
        

    let start (name : string) =
        let now = Performance.now()
        match activeGroup with
        | Some old ->
            addTime old (now - activeStart)
            suspended <- old :: suspended
        | None ->
            ()

        activeGroup <- Some name
        activeStart <- now

    let stop() =
        let now = Performance.now()
        match activeGroup with
        | Some old ->
            addTime old (now - activeStart)
            activeGroup <- None
            activeStart <- 0.0
            match suspended with
            | h :: t ->
                activeGroup <- Some h
                activeStart <- now
                suspended <- t
            | [] ->
                ()
        | None ->
            ()
        


        



    let addCallback c =
        callbacks <- c :: callbacks

    let startRender() =
        rendersPending <- rendersPending + 1

    let stopRender() =
        rendersPending <- rendersPending - 1
        if rendersPending = 0 then
            for c in callbacks do
                c ()

    let getTimes() : Map<string, float> =
        let res = times.Keys |> Seq.map (fun k -> k, times?(k)) |> Map.ofSeq
        times <- obj()
        res

    let inline measure (group : string) (action : unit -> 'a) =
        start group
        let res = action()
        stop()
        res

    let private emptyDisposable = 
        { new System.IDisposable with
            member x.Dispose() = ()
        }

    let startMeasure (group : string) = 
        start group
        { new System.IDisposable with
            member x.Dispose() = stop()
        }


