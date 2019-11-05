namespace Fable.React.Adaptive

open Fable.React
open FSharp.Data.Adaptive
open Fable.JsHelpers

[<AutoOpen>]
module Tags =   
    open Fable.React.ReactiveComponents

    let private textComponent : FunctionComponent<State<aval<string>>> =
        let inline render { value = (text : aval<string>) } =
            let text = Hooks.useAdaptive text
            Log.line "text: %s" text
            str text
            
        FunctionComponent.Of(render, "astr", Unchecked.equals)

    let astr (text : aval<string>) = 
        AdaptiveComponent.string text 

    let inline a props children = AdaptiveComponent.create "a" props children
    let inline aabbr props children = AdaptiveComponent.create "abbr" props children
    let inline aaddress props children = AdaptiveComponent.create "address" props children
    let inline aarticle props children = AdaptiveComponent.create "article" props children
    let inline aaside props children = AdaptiveComponent.create "aside" props children
    let inline aaudio props children = AdaptiveComponent.create "audio" props children
    let inline ab props children = AdaptiveComponent.create "b" props children
    let inline abdi props children = AdaptiveComponent.create "bdi" props children
    let inline abdo props children = AdaptiveComponent.create "bdo" props children
    let inline abig props children = AdaptiveComponent.create "big" props children
    let inline ablockquote props children = AdaptiveComponent.create "blockquote" props children
    let inline abody props children = AdaptiveComponent.create "body" props children
    let inline abutton props children = AdaptiveComponent.create "button" props children
    let inline acanvas props children = AdaptiveComponent.create "canvas" props children
    let inline acaption props children = AdaptiveComponent.create "caption" props children
    let inline acite props children = AdaptiveComponent.create "cite" props children
    let inline acode props children = AdaptiveComponent.create "code" props children
    let inline acolgroup props children = AdaptiveComponent.create "colgroup" props children
    let inline adata props children = AdaptiveComponent.create "data" props children
    let inline adatalist props children = AdaptiveComponent.create "datalist" props children
    let inline add props children = AdaptiveComponent.create "dd" props children
    let inline adel props children = AdaptiveComponent.create "del" props children
    let inline adetails props children = AdaptiveComponent.create "details" props children
    let inline adfn props children = AdaptiveComponent.create "dfn" props children
    let inline adialog props children = AdaptiveComponent.create "dialog" props children
    let inline adiv props children = AdaptiveComponent.create "div" props children
    let inline adl props children = AdaptiveComponent.create "dl" props children
    let inline adt props children = AdaptiveComponent.create "dt" props children
    let inline aem props children = AdaptiveComponent.create "em" props children
    let inline afieldset props children = AdaptiveComponent.create "fieldset" props children
    let inline afigcaption props children = AdaptiveComponent.create "figcaption" props children
    let inline afigure props children = AdaptiveComponent.create "figure" props children
    let inline afooter props children = AdaptiveComponent.create "footer" props children
    let inline aform props children = AdaptiveComponent.create "form" props children
    let inline ah1 props children = AdaptiveComponent.create "h1" props children
    let inline ah2 props children = AdaptiveComponent.create "h2" props children
    let inline ah3 props children = AdaptiveComponent.create "h3" props children
    let inline ah4 props children = AdaptiveComponent.create "h4" props children
    let inline ah5 props children = AdaptiveComponent.create "h5" props children
    let inline ah6 props children = AdaptiveComponent.create "h6" props children
    let inline ahead props children = AdaptiveComponent.create "head" props children
    let inline aheader props children = AdaptiveComponent.create "header" props children
    let inline ahgroup props children = AdaptiveComponent.create "hgroup" props children
    let inline ahtml props children = AdaptiveComponent.create "html" props children
    let inline ai props children = AdaptiveComponent.create "i" props children
    let inline aiframe props children = AdaptiveComponent.create "iframe" props children
    let inline ains props children = AdaptiveComponent.create "ins" props children
    let inline akbd props children = AdaptiveComponent.create "kbd" props children
    let inline alabel props children = AdaptiveComponent.create "label" props children
    let inline alegend props children = AdaptiveComponent.create "legend" props children
    let inline ali props children = AdaptiveComponent.create "li" props children
    let inline amain props children = AdaptiveComponent.create "main" props children
    let inline amap props children = AdaptiveComponent.create "map" props children
    let inline amark props children = AdaptiveComponent.create "mark" props children
    let inline amenu props children = AdaptiveComponent.create "menu" props children
    let inline ameter props children = AdaptiveComponent.create "meter" props children
    let inline anav props children = AdaptiveComponent.create "nav" props children
    let inline anoscript props children = AdaptiveComponent.create "noscript" props children
    let inline aobject props children = AdaptiveComponent.create "object" props children
    let inline aol props children = AdaptiveComponent.create "ol" props children
    let inline aoptgroup props children = AdaptiveComponent.create "optgroup" props children
    let inline aoption props children = AdaptiveComponent.create "option" props children
    let inline aoutput props children = AdaptiveComponent.create "output" props children
    let inline ap props children = AdaptiveComponent.create "p" props children
    let inline apicture props children = AdaptiveComponent.create "picture" props children
    let inline apre props children = AdaptiveComponent.create "pre" props children
    let inline aprogress props children = AdaptiveComponent.create "progress" props children
    let inline aq props children = AdaptiveComponent.create "q" props children
    let inline arp props children = AdaptiveComponent.create "rp" props children
    let inline art props children = AdaptiveComponent.create "rt" props children
    let inline aruby props children = AdaptiveComponent.create "ruby" props children
    let inline ``as`` props children = AdaptiveComponent.create "s" props children
    let inline asamp props children = AdaptiveComponent.create "samp" props children
    let inline ascript props children = AdaptiveComponent.create "script" props children
    let inline asection props children = AdaptiveComponent.create "section" props children
    let inline aselect props children = AdaptiveComponent.create "select" props children
    let inline asmall props children = AdaptiveComponent.create "small" props children
    let inline aspan props children = AdaptiveComponent.create "span" props children
    let inline astrong props children = AdaptiveComponent.create "strong" props children
    let inline astyle props children = AdaptiveComponent.create "style" props children
    let inline asub props children = AdaptiveComponent.create "sub" props children
    let inline asummary props children = AdaptiveComponent.create "summary" props children
    let inline asup props children = AdaptiveComponent.create "sup" props children
    let inline atable props children = AdaptiveComponent.create "table" props children
    let inline atbody props children = AdaptiveComponent.create "tbody" props children
    let inline atd props children = AdaptiveComponent.create "td" props children
    let inline atextarea props children = AdaptiveComponent.create "textarea" props children
    let inline atfoot props children = AdaptiveComponent.create "tfoot" props children
    let inline ath props children = AdaptiveComponent.create "th" props children
    let inline athead props children = AdaptiveComponent.create "thead" props children
    let inline atime props children = AdaptiveComponent.create "time" props children
    let inline atitle props children = AdaptiveComponent.create "title" props children
    let inline atr props children = AdaptiveComponent.create "tr" props children
    let inline au props children = AdaptiveComponent.create "u" props children
    let inline aul props children = AdaptiveComponent.create "ul" props children
    let inline avar props children = AdaptiveComponent.create "var" props children
    let inline avideo props children = AdaptiveComponent.create "video" props children

    // Void element
    let inline aarea props = AdaptiveComponent.create "area" props AList.empty
    let inline abase props = AdaptiveComponent.create "base" props AList.empty
    let inline abr props = AdaptiveComponent.create "br" props AList.empty
    let inline acol props = AdaptiveComponent.create "col" props AList.empty
    let inline aembed props = AdaptiveComponent.create "embed" props AList.empty
    let inline ahr props = AdaptiveComponent.create "hr" props AList.empty
    let inline aimg props = AdaptiveComponent.create "img" props AList.empty
    let inline ainput props = AdaptiveComponent.create "input" props AList.empty
    let inline akeygen props = AdaptiveComponent.create "keygen" props AList.empty
    let inline alink props = AdaptiveComponent.create "link" props AList.empty
    let inline amenuitem props = AdaptiveComponent.create "menuitem" props AList.empty
    let inline ameta props = AdaptiveComponent.create "meta" props AList.empty
    let inline aparam props = AdaptiveComponent.create "param" props AList.empty
    let inline asource props =AdaptiveComponent.create "source" props AList.empty
    let inline atrack props = AdaptiveComponent.create "track" props AList.empty
    let inline awbr props = AdaptiveComponent.create "wbr" props AList.empty
    let inline asvg props children = AdaptiveComponent.create "svg" props children
    let inline acircle props children = AdaptiveComponent.create "circle" props children
    let inline aclipPath props children = AdaptiveComponent.create "clipPath" props children
    let inline adefs props children = AdaptiveComponent.create "defs" props children
    let inline aellipse props children = AdaptiveComponent.create "ellipse" props children
    let inline ag props children = AdaptiveComponent.create "g" props children
    let inline aimage props children = AdaptiveComponent.create "image" props children
    let inline aline props children = AdaptiveComponent.create "line" props children
    let inline alinearGradient props children = AdaptiveComponent.create "linearGradient" props children
    let inline amask props children = AdaptiveComponent.create "mask" props children
    let inline apath props children = AdaptiveComponent.create "path" props children
    let inline apattern props children = AdaptiveComponent.create "pattern" props children
    let inline apolygon props children = AdaptiveComponent.create "polygon" props children
    let inline apolyline props children = AdaptiveComponent.create "polyline" props children
    let inline aradialGradient props children = AdaptiveComponent.create "radialGradient" props children
    let inline arect props children = AdaptiveComponent.create "rect" props children
    let inline astop props children = AdaptiveComponent.create "stop" props children
    let inline atext props children = AdaptiveComponent.create "text" props children
    let inline atspan props children = AdaptiveComponent.create "tspan" props children
[<AutoOpen>]
module Debugging =
    let inline withLogging (name : string) (e : ReactElement) = 
        LogComponent.create name e

