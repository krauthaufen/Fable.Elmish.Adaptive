### Fable.Elmish.Adaptive

[![CI - Linux](https://github.com/krauthaufen/Fable.Elmish.Adaptive/workflows/CI%20-%20Linux/badge.svg)](https://github.com/krauthaufen/Fable.Elmish.Adaptive/actions?query=workflow%3A%22CI+-+Linux%22)
[![CI - Windows](https://github.com/krauthaufen/Fable.Elmish.Adaptive/workflows/CI%20-%20Windows/badge.svg)](https://github.com/krauthaufen/Fable.Elmish.Adaptive/actions?query=workflow%3A%22CI+-+Windows%22)

Note that I'm currently experimenting with performance improvements, so until this is finished quality and performance may vary...

Aims to provide adaptive bindings for ELM-style UI in Fable.  
In order to keep the view-code as compatible as possible our **AdaptiveComponents** are 
directly implemented as `ReactComponent` and internally manage their DOM (directly modifying it without react knowing).
Nonetheless it is worth mentioning that the component uses react for rendering its children again, maximizing compatibiity and leveraging react's reliable update logic. 

If you have any questions or want to get involved just join us on discord: [![Discord](https://discordapp.com/api/guilds/611129394764840960/widget.png)](https://discord.gg/UyecnhM)

### Building
the Demo project can be started using `build -t Watch` (starting a webpack-dev-server) or `build -t Run(Debug|Release)` (bundling the demo project and starting a web-server in the output directory)

Switching between Demos currently requires changing `src/Demo/Program.fs` atm.

### Demos

The Demo project currently contains two examples:
* [SimpleApp.fs](https://github.com/krauthaufen/Fable.Elmish.Adaptive/blob/master/src/Demo/SimpleApp.fs) using elmish(ish) MVU updates.
  However since [Adaptify](https://github.com/krauthaufen/Adaptify) is not yet finished the example is limited to a *primitive* model.
* [AdaptiveListDemo.fs](https://github.com/krauthaufen/Fable.Elmish.Adaptive/blob/master/src/Demo/AdaptiveListDemo.fs) using Fable.React.Adaptive for rendering an `alist` of nodes with the possiblity to insert/change/remove nodes (not using MVU style updates atm.)
* [TodoMvc.fs](https://github.com/krauthaufen/Fable.Elmish.Adaptive/blob/master/src/Demo/TodoMvc.fs) is a port of fable's [react-todomvc](https://github.com/elmish/sample-react-todomvc) using our elmish(ish) view functions and [Adaptify](https://github.com/krauthaufen/Adaptify) for *adaptifying* its model. There's also a [live demo](https://aardvarkians.com/demo/TodoMVC/) which currenlty does not store the model in the `localstorage` due to serialization issues with our `IndexList` but we'll fix that eventually.

