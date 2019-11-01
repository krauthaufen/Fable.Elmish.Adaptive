namespace Fable.React.Adaptive

open Fable.Core.JsInterop
open Fable.React

module internal ComponentHelpers =
    let inline setDisplayName<'comp, 'props, 'state when 'comp :> Component<'props, 'state>> (name : string) =
        let t = ReactElementType.ofComponent<'comp, 'props, 'state>
        t?displayName <- name

    