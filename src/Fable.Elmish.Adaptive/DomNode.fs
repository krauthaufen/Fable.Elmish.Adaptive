namespace Fable.Elmish.Adaptive

open FSharp.Data.Adaptive

type Tag =  
    { 
        xmlns   : Option<string>
        name    : string
    } 

    static member inline Create (t : Tag, c : AttributeMap, str : string) = 
        Text (t, c, AVal.constant str)
            
    static member inline Create (t : Tag, c : AttributeMap, _ : unit) = 
        Void (t, c)
            
    static member inline Create (t : Tag, c : AttributeMap, str : aval<string>) = 
        Text (t, c, str)

    static member inline Create (t : Tag, c : AttributeMap, str : list<DomNode>) = 
        Inner (t, c, AList.ofList str)

    static member inline Create (t : Tag, c : AttributeMap, str : alist<DomNode>) = 
        Inner (t, c, str)

    static member inline Create (t : Tag, c : list<string * AttributeValue>, str : string) = 
        Text (t, AttributeMap.ofList c, AVal.constant str)
            
    static member inline Create (t : Tag, c : list<string * AttributeValue>, str : aval<string>) = 
        Text (t, AttributeMap.ofList c, str)

    static member inline Create (t : Tag, c : list<string * AttributeValue>, str : list<DomNode>) = 
        Inner (t, AttributeMap.ofList c, AList.ofList str)
            
    static member inline Create (t : Tag, c : list<string * AttributeValue>, str : alist<DomNode>) = 
        Inner (t, AttributeMap.ofList c, str)
            
    static member inline Create (t : Tag, c : list<string * AttributeValue>, str : unit) = 
        Void (t, AttributeMap.ofList c)

and DomNode =
    | Inner of tag : Tag * attributes : AttributeMap * children : alist<DomNode>
    | Text of tag : Tag * attributes : AttributeMap * content : aval<string>
    | Void of tag : Tag * attributes : AttributeMap

module DomNode =

    let inner (tag : Tag) (attributes : list<string * AttributeValue>) (content : list<DomNode>) =
        Inner(tag, AttributeMap.ofList attributes, AList.ofList content)
        
    let text (tag : Tag) (attributes : list<string * AttributeValue>) (content : string) =
        Text(tag, AttributeMap.ofList attributes, AVal.constant content)
        
    let empty (tag : Tag) (attributes : list<string * AttributeValue>) =
        Void(tag, AttributeMap.ofList attributes)

    let inline generic (d : ^d) (c : ^a) (v : ^b) : DomNode =
        ((^a or ^b or ^d) : (static member Create :  ^d * ^a * ^b -> DomNode) (d, c, v))
            
    let inline genericVoid (d : ^d) (c : ^a) = 
        generic d c ()

[<AutoOpen>]
module Attributes = 
    let inline clazz className = 
        "class", AttributeValue.String className

    let inline style style = 
        "style", AttributeValue.String style

    let inline click (cb : unit -> unit) = 
        "click", AttributeValue.Event [ { useCapture = false; callback = fun _ -> cb() } ]
            
    let inline mousemove (cb : unit -> unit) = 
        "mousemove", AttributeValue.Event [ { useCapture = false; callback = fun _ -> cb() } ]

[<AutoOpen>]
module Tags = 
    let inline str content = DomNode.text { xmlns = None; name = "span" } [] content

    let inline a props children = DomNode.inner { xmlns = None; name = "a" } props children
    let inline abbr props children = DomNode.inner { xmlns = None; name = "abbr" } props children
    let inline address props children = DomNode.inner { xmlns = None; name = "address" } props children
    let inline article props children = DomNode.inner { xmlns = None; name = "article" } props children
    let inline aside props children = DomNode.inner { xmlns = None; name = "aside" } props children
    let inline audio props children = DomNode.inner { xmlns = None; name = "audio" } props children
    let inline b props children = DomNode.inner { xmlns = None; name = "b" } props children
    let inline bdi props children = DomNode.inner { xmlns = None; name = "bdi" } props children
    let inline bdo props children = DomNode.inner { xmlns = None; name = "bdo" } props children
    let inline big props children = DomNode.inner { xmlns = None; name = "big" } props children
    let inline blockquote props children = DomNode.inner { xmlns = None; name = "blockquote" } props children
    let inline body props children = DomNode.inner { xmlns = None; name = "body" } props children
    let inline button props children = DomNode.inner { xmlns = None; name = "button" } props children
    let inline canvas props children = DomNode.inner { xmlns = None; name = "canvas" } props children
    let inline caption props children = DomNode.inner { xmlns = None; name = "caption" } props children
    let inline cite props children = DomNode.inner { xmlns = None; name = "cite" } props children
    let inline code props children = DomNode.inner { xmlns = None; name = "code" } props children
    let inline colgroup props children = DomNode.inner { xmlns = None; name = "colgroup" } props children
    let inline data props children = DomNode.inner { xmlns = None; name = "data" } props children
    let inline datalist props children = DomNode.inner { xmlns = None; name = "datalist" } props children
    let inline dd props children = DomNode.inner { xmlns = None; name = "dd" } props children
    let inline del props children = DomNode.inner { xmlns = None; name = "del" } props children
    let inline details props children = DomNode.inner { xmlns = None; name = "details" } props children
    let inline dfn props children = DomNode.inner { xmlns = None; name = "dfn" } props children
    let inline dialog props children = DomNode.inner { xmlns = None; name = "dialog" } props children
    let inline div props children = DomNode.inner { xmlns = None; name = "div" } props children
    let inline dl props children = DomNode.inner { xmlns = None; name = "dl" } props children
    let inline dt props children = DomNode.inner { xmlns = None; name = "dt" } props children
    let inline em props children = DomNode.inner { xmlns = None; name = "em" } props children
    let inline fieldset props children = DomNode.inner { xmlns = None; name = "fieldset" } props children
    let inline figcaption props children = DomNode.inner { xmlns = None; name = "figcaption" } props children
    let inline figure props children = DomNode.inner { xmlns = None; name = "figure" } props children
    let inline footer props children = DomNode.inner { xmlns = None; name = "footer" } props children
    let inline form props children = DomNode.inner { xmlns = None; name = "form" } props children
    let inline h1 props children = DomNode.inner { xmlns = None; name = "h1" } props children
    let inline h2 props children = DomNode.inner { xmlns = None; name = "h2" } props children
    let inline h3 props children = DomNode.inner { xmlns = None; name = "h3" } props children
    let inline h4 props children = DomNode.inner { xmlns = None; name = "h4" } props children
    let inline h5 props children = DomNode.inner { xmlns = None; name = "h5" } props children
    let inline h6 props children = DomNode.inner { xmlns = None; name = "h6" } props children
    let inline head props children = DomNode.inner { xmlns = None; name = "head" } props children
    let inline header props children = DomNode.inner { xmlns = None; name = "header" } props children
    let inline hgroup props children = DomNode.inner { xmlns = None; name = "hgroup" } props children
    let inline html props children = DomNode.inner { xmlns = None; name = "html" } props children
    let inline i props children = DomNode.inner { xmlns = None; name = "i" } props children
    let inline iframe props children = DomNode.inner { xmlns = None; name = "iframe" } props children
    let inline ins props children = DomNode.inner { xmlns = None; name = "ins" } props children
    let inline kbd props children = DomNode.inner { xmlns = None; name = "kbd" } props children
    let inline label props children = DomNode.inner { xmlns = None; name = "label" } props children
    let inline legend props children = DomNode.inner { xmlns = None; name = "legend" } props children
    let inline li props children = DomNode.inner { xmlns = None; name = "li" } props children
    let inline main props children = DomNode.inner { xmlns = None; name = "main" } props children
    let inline map props children = DomNode.inner { xmlns = None; name = "map" } props children
    let inline mark props children = DomNode.inner { xmlns = None; name = "mark" } props children
    let inline menu props children = DomNode.inner { xmlns = None; name = "menu" } props children
    let inline meter props children = DomNode.inner { xmlns = None; name = "meter" } props children
    let inline nav props children = DomNode.inner { xmlns = None; name = "nav" } props children
    let inline noscript props children = DomNode.inner { xmlns = None; name = "noscript" } props children
    let inline object props children = DomNode.inner { xmlns = None; name = "object" } props children
    let inline ol props children = DomNode.inner { xmlns = None; name = "ol" } props children
    let inline optgroup props children = DomNode.inner { xmlns = None; name = "optgroup" } props children
    let inline option props children = DomNode.inner { xmlns = None; name = "option" } props children
    let inline output props children = DomNode.inner { xmlns = None; name = "output" } props children
    let inline p props children = DomNode.inner { xmlns = None; name = "p" } props children
    let inline picture props children = DomNode.inner { xmlns = None; name = "picture" } props children
    let inline pre props children = DomNode.inner { xmlns = None; name = "pre" } props children
    let inline progress props children = DomNode.inner { xmlns = None; name = "progress" } props children
    let inline q props children = DomNode.inner { xmlns = None; name = "q" } props children
    let inline rp props children = DomNode.inner { xmlns = None; name = "rp" } props children
    let inline rt props children = DomNode.inner { xmlns = None; name = "rt" } props children
    let inline ruby props children = DomNode.inner { xmlns = None; name = "ruby" } props children
    let inline s props children = DomNode.inner { xmlns = None; name = "s" } props children
    let inline samp props children = DomNode.inner { xmlns = None; name = "samp" } props children
    let inline script props children = DomNode.inner { xmlns = None; name = "script" } props children
    let inline section props children = DomNode.inner { xmlns = None; name = "section" } props children
    let inline select props children = DomNode.inner { xmlns = None; name = "select" } props children
    let inline small props children = DomNode.inner { xmlns = None; name = "small" } props children
    let inline span props children = DomNode.inner { xmlns = None; name = "span" } props children
    let inline strong props children = DomNode.inner { xmlns = None; name = "strong" } props children
    let inline style props children = DomNode.inner { xmlns = None; name = "style" } props children
    let inline sub props children = DomNode.inner { xmlns = None; name = "sub" } props children
    let inline summary props children = DomNode.inner { xmlns = None; name = "summary" } props children
    let inline sup props children = DomNode.inner { xmlns = None; name = "sup" } props children
    let inline table props children = DomNode.inner { xmlns = None; name = "table" } props children
    let inline tbody props children = DomNode.inner { xmlns = None; name = "tbody" } props children
    let inline td props children = DomNode.inner { xmlns = None; name = "td" } props children
    let inline textarea props children = DomNode.inner { xmlns = None; name = "textarea" } props children
    let inline tfoot props children = DomNode.inner { xmlns = None; name = "tfoot" } props children
    let inline th props children = DomNode.inner { xmlns = None; name = "th" } props children
    let inline thead props children = DomNode.inner { xmlns = None; name = "thead" } props children
    let inline time props children = DomNode.inner { xmlns = None; name = "time" } props children
    let inline title props children = DomNode.inner { xmlns = None; name = "title" } props children
    let inline tr props children = DomNode.inner { xmlns = None; name = "tr" } props children
    let inline u props children = DomNode.inner { xmlns = None; name = "u" } props children
    let inline ul props children = DomNode.inner { xmlns = None; name = "ul" } props children
    let inline var props children = DomNode.inner { xmlns = None; name = "var" } props children
    let inline video props children = DomNode.inner { xmlns = None; name = "video" } props children

    // Void element
    let inline area props = DomNode.empty { xmlns = None; name = "area" } props
    let inline ``base`` props = DomNode.empty { xmlns = None; name = "base" } props
    let inline br props = DomNode.empty { xmlns = None; name = "br" } props
    let inline col props = DomNode.empty { xmlns = None; name = "col" } props
    let inline embed props = DomNode.empty { xmlns = None; name = "embed" } props
    let inline hr props = DomNode.empty { xmlns = None; name = "hr" } props
    let inline img props = DomNode.empty { xmlns = None; name = "img" } props
    let inline input props = DomNode.empty { xmlns = None; name = "input" } props
    let inline keygen props = DomNode.empty { xmlns = None; name = "keygen" } props
    let inline link props = DomNode.empty { xmlns = None; name = "link" } props
    let inline menuitem props = DomNode.empty { xmlns = None; name = "menuitem" } props
    let inline meta props = DomNode.empty { xmlns = None; name = "meta" } props
    let inline param props = DomNode.empty { xmlns = None; name = "param" } props
    let inline source props = DomNode.empty { xmlns = None; name = "source" } props
    let inline track props = DomNode.empty { xmlns = None; name = "track" } props
    let inline wbr props = DomNode.empty { xmlns = None; name = "wbr" } props

    // SVG elements
    let inline svg props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "svg" } props children
    let inline circle props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "circle" } props children
    let inline clipPath props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "clipPath" } props children
    let inline defs props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "defs" } props children
    let inline ellipse props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "ellipse" } props children
    let inline g props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "g" } props children
    let inline image props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "image" } props children
    let inline line props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "line" } props children
    let inline linearGradient props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "linearGradient" } props children
    let inline mask props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "mask" } props children
    let inline path props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "path" } props children
    let inline pattern props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "pattern" } props children
    let inline polygon props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "polygon" } props children
    let inline polyline props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "polyline" } props children
    let inline radialGradient props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "radialGradient" } props children
    let inline rect props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "rect" } props children
    let inline stop props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "stop" } props children
    let inline text props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "text" } props children
    let inline tspan props children = DomNode.inner { xmlns = Some "http://www.w3.org/2000/svg"; name = "tspan" } props children
    
module Generic =

    [<AutoOpen>]
    module Tags = 
        let inline str content = DomNode.generic { xmlns = None; name = "span" } [] content

        let inline a props children = DomNode.generic { xmlns = None; name = "a" } props children
        let inline abbr props children = DomNode.generic { xmlns = None; name = "abbr" } props children
        let inline address props children = DomNode.generic { xmlns = None; name = "address" } props children
        let inline article props children = DomNode.generic { xmlns = None; name = "article" } props children
        let inline aside props children = DomNode.generic { xmlns = None; name = "aside" } props children
        let inline audio props children = DomNode.generic { xmlns = None; name = "audio" } props children
        let inline b props children = DomNode.generic { xmlns = None; name = "b" } props children
        let inline bdi props children = DomNode.generic { xmlns = None; name = "bdi" } props children
        let inline bdo props children = DomNode.generic { xmlns = None; name = "bdo" } props children
        let inline big props children = DomNode.generic { xmlns = None; name = "big" } props children
        let inline blockquote props children = DomNode.generic { xmlns = None; name = "blockquote" } props children
        let inline body props children = DomNode.generic { xmlns = None; name = "body" } props children
        let inline button props children = DomNode.generic { xmlns = None; name = "button" } props children
        let inline canvas props children = DomNode.generic { xmlns = None; name = "canvas" } props children
        let inline caption props children = DomNode.generic { xmlns = None; name = "caption" } props children
        let inline cite props children = DomNode.generic { xmlns = None; name = "cite" } props children
        let inline code props children = DomNode.generic { xmlns = None; name = "code" } props children
        let inline colgroup props children = DomNode.generic { xmlns = None; name = "colgroup" } props children
        let inline data props children = DomNode.generic { xmlns = None; name = "data" } props children
        let inline datalist props children = DomNode.generic { xmlns = None; name = "datalist" } props children
        let inline dd props children = DomNode.generic { xmlns = None; name = "dd" } props children
        let inline del props children = DomNode.generic { xmlns = None; name = "del" } props children
        let inline details props children = DomNode.generic { xmlns = None; name = "details" } props children
        let inline dfn props children = DomNode.generic { xmlns = None; name = "dfn" } props children
        let inline dialog props children = DomNode.generic { xmlns = None; name = "dialog" } props children
        let inline div props children = DomNode.generic { xmlns = None; name = "div" } props children
        let inline dl props children = DomNode.generic { xmlns = None; name = "dl" } props children
        let inline dt props children = DomNode.generic { xmlns = None; name = "dt" } props children
        let inline em props children = DomNode.generic { xmlns = None; name = "em" } props children
        let inline fieldset props children = DomNode.generic { xmlns = None; name = "fieldset" } props children
        let inline figcaption props children = DomNode.generic { xmlns = None; name = "figcaption" } props children
        let inline figure props children = DomNode.generic { xmlns = None; name = "figure" } props children
        let inline footer props children = DomNode.generic { xmlns = None; name = "footer" } props children
        let inline form props children = DomNode.generic { xmlns = None; name = "form" } props children
        let inline h1 props children = DomNode.generic { xmlns = None; name = "h1" } props children
        let inline h2 props children = DomNode.generic { xmlns = None; name = "h2" } props children
        let inline h3 props children = DomNode.generic { xmlns = None; name = "h3" } props children
        let inline h4 props children = DomNode.generic { xmlns = None; name = "h4" } props children
        let inline h5 props children = DomNode.generic { xmlns = None; name = "h5" } props children
        let inline h6 props children = DomNode.generic { xmlns = None; name = "h6" } props children
        let inline head props children = DomNode.generic { xmlns = None; name = "head" } props children
        let inline header props children = DomNode.generic { xmlns = None; name = "header" } props children
        let inline hgroup props children = DomNode.generic { xmlns = None; name = "hgroup" } props children
        let inline html props children = DomNode.generic { xmlns = None; name = "html" } props children
        let inline i props children = DomNode.generic { xmlns = None; name = "i" } props children
        let inline iframe props children = DomNode.generic { xmlns = None; name = "iframe" } props children
        let inline ins props children = DomNode.generic { xmlns = None; name = "ins" } props children
        let inline kbd props children = DomNode.generic { xmlns = None; name = "kbd" } props children
        let inline label props children = DomNode.generic { xmlns = None; name = "label" } props children
        let inline legend props children = DomNode.generic { xmlns = None; name = "legend" } props children
        let inline li props children = DomNode.generic { xmlns = None; name = "li" } props children
        let inline main props children = DomNode.generic { xmlns = None; name = "main" } props children
        let inline map props children = DomNode.generic { xmlns = None; name = "map" } props children
        let inline mark props children = DomNode.generic { xmlns = None; name = "mark" } props children
        let inline menu props children = DomNode.generic { xmlns = None; name = "menu" } props children
        let inline meter props children = DomNode.generic { xmlns = None; name = "meter" } props children
        let inline nav props children = DomNode.generic { xmlns = None; name = "nav" } props children
        let inline noscript props children = DomNode.generic { xmlns = None; name = "noscript" } props children
        let inline object props children = DomNode.generic { xmlns = None; name = "object" } props children
        let inline ol props children = DomNode.generic { xmlns = None; name = "ol" } props children
        let inline optgroup props children = DomNode.generic { xmlns = None; name = "optgroup" } props children
        let inline option props children = DomNode.generic { xmlns = None; name = "option" } props children
        let inline output props children = DomNode.generic { xmlns = None; name = "output" } props children
        let inline p props children = DomNode.generic { xmlns = None; name = "p" } props children
        let inline picture props children = DomNode.generic { xmlns = None; name = "picture" } props children
        let inline pre props children = DomNode.generic { xmlns = None; name = "pre" } props children
        let inline progress props children = DomNode.generic { xmlns = None; name = "progress" } props children
        let inline q props children = DomNode.generic { xmlns = None; name = "q" } props children
        let inline rp props children = DomNode.generic { xmlns = None; name = "rp" } props children
        let inline rt props children = DomNode.generic { xmlns = None; name = "rt" } props children
        let inline ruby props children = DomNode.generic { xmlns = None; name = "ruby" } props children
        let inline s props children = DomNode.generic { xmlns = None; name = "s" } props children
        let inline samp props children = DomNode.generic { xmlns = None; name = "samp" } props children
        let inline script props children = DomNode.generic { xmlns = None; name = "script" } props children
        let inline section props children = DomNode.generic { xmlns = None; name = "section" } props children
        let inline select props children = DomNode.generic { xmlns = None; name = "select" } props children
        let inline small props children = DomNode.generic { xmlns = None; name = "small" } props children
        let inline span props children = DomNode.generic { xmlns = None; name = "span" } props children
        let inline strong props children = DomNode.generic { xmlns = None; name = "strong" } props children
        let inline style props children = DomNode.generic { xmlns = None; name = "style" } props children
        let inline sub props children = DomNode.generic { xmlns = None; name = "sub" } props children
        let inline summary props children = DomNode.generic { xmlns = None; name = "summary" } props children
        let inline sup props children = DomNode.generic { xmlns = None; name = "sup" } props children
        let inline table props children = DomNode.generic { xmlns = None; name = "table" } props children
        let inline tbody props children = DomNode.generic { xmlns = None; name = "tbody" } props children
        let inline td props children = DomNode.generic { xmlns = None; name = "td" } props children
        let inline textarea props children = DomNode.generic { xmlns = None; name = "textarea" } props children
        let inline tfoot props children = DomNode.generic { xmlns = None; name = "tfoot" } props children
        let inline th props children = DomNode.generic { xmlns = None; name = "th" } props children
        let inline thead props children = DomNode.generic { xmlns = None; name = "thead" } props children
        let inline time props children = DomNode.generic { xmlns = None; name = "time" } props children
        let inline title props children = DomNode.generic { xmlns = None; name = "title" } props children
        let inline tr props children = DomNode.generic { xmlns = None; name = "tr" } props children
        let inline u props children = DomNode.generic { xmlns = None; name = "u" } props children
        let inline ul props children = DomNode.generic { xmlns = None; name = "ul" } props children
        let inline var props children = DomNode.generic { xmlns = None; name = "var" } props children
        let inline video props children = DomNode.generic { xmlns = None; name = "video" } props children

        // Void element
        let inline area props = DomNode.genericVoid { xmlns = None; name = "area" } props
        let inline ``base`` props = DomNode.genericVoid { xmlns = None; name = "base" } props
        let inline br props = DomNode.genericVoid { xmlns = None; name = "br" } props
        let inline col props = DomNode.genericVoid { xmlns = None; name = "col" } props
        let inline embed props = DomNode.genericVoid { xmlns = None; name = "embed" } props
        let inline hr props = DomNode.genericVoid { xmlns = None; name = "hr" } props
        let inline img props = DomNode.genericVoid { xmlns = None; name = "img" } props
        let inline input props = DomNode.genericVoid { xmlns = None; name = "input" } props
        let inline keygen props = DomNode.genericVoid { xmlns = None; name = "keygen" } props
        let inline link props = DomNode.genericVoid { xmlns = None; name = "link" } props
        let inline menuitem props = DomNode.genericVoid { xmlns = None; name = "menuitem" } props
        let inline meta props = DomNode.genericVoid { xmlns = None; name = "meta" } props
        let inline param props = DomNode.genericVoid { xmlns = None; name = "param" } props
        let inline source props = DomNode.genericVoid { xmlns = None; name = "source" } props
        let inline track props = DomNode.genericVoid { xmlns = None; name = "track" } props
        let inline wbr props = DomNode.genericVoid { xmlns = None; name = "wbr" } props

        // SVG elements
        let inline svg props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "svg" } props children
        let inline circle props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "circle" } props children
        let inline clipPath props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "clipPath" } props children
        let inline defs props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "defs" } props children
        let inline ellipse props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "ellipse" } props children
        let inline g props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "g" } props children
        let inline image props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "image" } props children
        let inline line props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "line" } props children
        let inline linearGradient props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "linearGradient" } props children
        let inline mask props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "mask" } props children
        let inline path props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "path" } props children
        let inline pattern props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "pattern" } props children
        let inline polygon props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "polygon" } props children
        let inline polyline props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "polyline" } props children
        let inline radialGradient props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "radialGradient" } props children
        let inline rect props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "rect" } props children
        let inline stop props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "stop" } props children
        let inline text props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "text" } props children
        let inline tspan props children = DomNode.generic { xmlns = Some "http://www.w3.org/2000/svg"; name = "tspan" } props children
    
    
