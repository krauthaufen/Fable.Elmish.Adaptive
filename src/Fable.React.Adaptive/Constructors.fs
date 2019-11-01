namespace Fable.React.Adaptive

open Fable.React
        
[<AutoOpen>]
module AdaptiveTags =
    let inline adiv c = AListComponent.ofAlist "div" c
    let inline aol c = AListComponent.ofAlist "ol" c
    let inline aul c = AListComponent.ofAlist "ul" c

    let inline withLogging (name : string) (e : ReactElement) = 
        LogComponent.create name e

