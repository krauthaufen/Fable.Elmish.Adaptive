### Fable.Elmish.Adaptive


Aims to provide adaptive bindings for ELM-style UI in Fable.  
In order to keep the view-code as compatible as possible our **AdaptiveComponents** are 
directly implemented as `ReactComponent` and internally manage their DOM (directly modifying it without react knowing).
Nonetheless it is worth mentioning that the component uses react for rendering its children again, maximizing compatibiity and leveraging react's reliable update logic. 

If you have any questions or want to get involved just join us on discord: [![Discord](https://discordapp.com/api/guilds/611129394764840960/widget.png)](https://discord.gg/UyecnhM)

### Demos

The Demo project currently contains two examples:
* [SimpleApp.fs](https://github.com/krauthaufen/Fable.Elmish.Adaptive/blob/master/src/Demo/SimpleApp.fs) using elmish(ish) MVU updates.
  However since [Adaptify](https://github.com/krauthaufen/Adaptify) is not yet finished the example is limited to a *primitive* model.
* [AdaptiveListDemo.fs](https://github.com/krauthaufen/Fable.Elmish.Adaptive/blob/master/src/Demo/AdaptiveListDemo.fs) using Fable.React.Adaptive for rendering an `alist` of nodes with the possiblity to insert/change/remove nodes (not using MVU style updates atm.)
* [TodoMvc.fs](https://github.com/krauthaufen/Fable.Elmish.Adaptive/blob/master/src/Demo/TodoMvc.fs) is a port of fable's [react-todomvc](https://github.com/elmish/sample-react-todomvc) using our elmish(ish) view functions and [Adaptify](https://github.com/krauthaufen/Adaptify) for *adaptifying* its model. There's also a [live demo](https://aardvarkians.com/demo/TodoMVC/) which currenlty does not store the model in the `localstorage` due to serialization issues with our `IndexList` but we'll fix that eventually.

