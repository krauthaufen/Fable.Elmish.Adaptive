namespace Fable.React.Adaptive

open Fable.React
        
[<AutoOpen>]
module AdaptiveTags =
    let inline adiv c = AListComponent.ofAList "div" c
    let inline aol c = AListComponent.ofAList "ol" c
    let inline aul c = AListComponent.ofAList "ul" c

    let inline withLogging (name : string) (e : ReactElement) = 
        LogComponent.create name e

