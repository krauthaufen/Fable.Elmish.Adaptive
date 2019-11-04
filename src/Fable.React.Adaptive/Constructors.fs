namespace Fable.React.Adaptive

open Fable.React
        
[<AutoOpen>]
module AdaptiveTags =
    let inline adiv att children = AListComponent.ofAList "div" att children
    let inline aol att children = AListComponent.ofAList "ol" att children
    let inline aul att children = AListComponent.ofAList "ul" att children

    let inline withLogging (name : string) (e : ReactElement) = 
        LogComponent.create name e

