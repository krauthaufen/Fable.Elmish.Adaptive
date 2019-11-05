namespace Fable.React.Adaptive

open Browser
open Fable.React
open Fable.React.ReactiveComponents
open Fable.JsHelpers

type internal LogComponent(value : State<string * ReactElement>) =
    inherit Component<State<string * ReactElement>, State<string * ReactElement>>(value)
    static do ComponentHelpers.setDisplayName<LogComponent,_,_> "Log"
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

