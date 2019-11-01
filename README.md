### Fable.Elmish.Adaptive


Aims to provide adaptive bindings for ELM-style UI in Fable.
Please note that this is a very early prototype and I'm currently working on `Fable.React.Adaptive` which should allow users to
integrate adaptive things (`aval`, `aset`, etc.) in a react-dom. 
As this is pretty much orthogonal to using ELM style updates I started creating a lib called `Fable.React.Adaptive` aiming to provide adaptive components that can also be used outside of Elmish-World.

If you have any questions or want to get involved just join us on discord: [![Discord](https://discordapp.com/api/guilds/611129394764840960/widget.png)](https://discord.gg/UyecnhM)

### Demos

The Demo project currently contains two examples:
* [SimpleApp.fs](https://github.com/krauthaufen/Fable.Elmish.Adaptive/blob/master/src/Demo/SimpleApp.fs) using elmish(ish) MVU updates.
  However since [Adaptify](https://github.com/krauthaufen/Adaptify) is not yet finished the example is limited to a *primitive* model.
* [AdaptiveListDemo.fs](https://github.com/krauthaufen/Fable.Elmish.Adaptive/blob/master/src/Demo/AdaptiveListDemo.fs) using Fable.React.Adaptive for rendering an `alist` of nodes with the possiblity to insert/change/remove nodes (not using MVU style updates atm.)

#### Roadmap
* complete `Fable.React.Adaptive`
* create `Fable.Elmish.Adaptive` providing proper app/view abstractions (similar to Elmish itself)
