module SvgTargets.Svg
open FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Xml
open System.Xml.Linq
open System

type SvgConfiguration =
    {   IncludeRuler : bool
        IncludeInfo : bool
        RingThickness : float<m>
        BlackOverride : string option
        WhiteOverride : string option
        DrawBorder : bool
    }
    static member Default =
        {   IncludeRuler = false
            IncludeInfo = true
            RingThickness = 0.0008<m>
            BlackOverride = None
            WhiteOverride = None
            DrawBorder = true
        }

let private attr (name : string) (value : string) = XAttribute(XName.Get(name), value)

let private mm (size : float<m>) = (size * 1000.0<1/m>).ToString("0.00")

let private clockToRadians (clock : float) =
    clock * (360.0/12.0) * (Math.PI/180.0)

let private offsetClock (centerX : float<m>, centerY : float<m>) (clock : float) (radius : float<m>) =
    let rads = clockToRadians clock
    let xOffset = sin rads * radius
    let yOffset = -cos rads * radius
    (centerX + xOffset, centerY + yOffset)

let renderTarget (config : SvgConfiguration) (target : TargetDefinition) =
    let svgNamespace = XNamespace.Get("http://www.w3.org/2000/svg")
    let xn (n : string) : XName = svgNamespace + n
    let color (fill : RingFill) =
        match fill with
        | Black -> defaultArg config.BlackOverride "black"
        | White -> defaultArg config.WhiteOverride "white"
    let centerX, centerY =
        let w, h = target.PaperSize
        w * 0.5, h * 0.5
    let bigToSmall = target.Rings |> Array.sortByDescending (fun r -> r.Radius)
    let rings =
        [|  for i, ring in bigToSmall |> Seq.indexed do
                let largerRing =
                    if i > 0 then Some bigToSmall.[i-1] else None
                // The stroke will expand the effective radius of the ring, so we make the circle radius a little smaller.
                // Goal is to have the OUTSIDE edge of the ring, including stroke, match exactly the defined size for scoring purposes.
                let dotSizeRing = 0.0001<m>
                let circleRadius = max dotSizeRing (ring.Radius - config.RingThickness * 0.5)
                let strokeColor =
                    let outerColor =
                        match largerRing with
                        | None -> White
                        | Some r -> r.Fill
                    outerColor.ContrastColor
                yield XElement(xn "circle"
                    , attr "id" ("circle-ring-" + ring.Label)
                    , attr "cx" (mm centerX)
                    , attr "cy" (mm centerY)
                    , attr "r" (mm circleRadius)
                    , attr "stroke-width" (mm config.RingThickness)
                    , attr "stroke" (color strokeColor)
                    , attr "fill" (if circleRadius <= dotSizeRing then color strokeColor else (color ring.Fill))
                    )
        |]
    let ruler =
        let dimeDiameter = 0.01791<m>
        let stroke = 0.0005<m>
        let radius = dimeDiameter * 0.5 - stroke * 0.5
        XElement(xn "g"
            , XElement(xn "circle"
                , attr "id" "ruler-circle"
                , attr "cx" (mm dimeDiameter)
                , attr "cy" (mm (snd target.PaperSize - dimeDiameter))
                , attr "r" (mm radius)
                , attr "stroke-width" (mm stroke)
                , attr "stroke" (color Black)
                , attr "fill" (color White)
                )
            , XElement(xn "text"
                , attr "id" "ruler-info-1"
                , attr "x" (mm dimeDiameter)
                , attr "y" (mm (snd target.PaperSize - dimeDiameter))
                , attr "text-anchor" "middle"
                , attr "dominant-baseline" "central"
                , attr "stroke" "none"
                , attr "fill" (color Black)
                , attr "font-size" (mm 0.003<m>)
                , attr "font-family" "sans-serif"
                , "O.D. = 10¢"
                )
            )
    let info =
        let infoText = target.Identifier + " | " + target.Name + " | SVGTargets.com"
        XElement(xn "text"
            , attr "id" "target-info"
            , attr "x" (mm centerX)
            , attr "y" (mm (snd target.PaperSize - 0.006<m>))
            , attr "text-anchor" "middle"
            , attr "dominant-baseline" "central"
            , attr "stroke" "none"
            , attr "fill" (color Black)
            , attr "font-size" (mm 0.003<m>)
            , attr "font-family" "sans-serif"
            , infoText
            )
    let labels =
        let minGapForLabel = 0.004<m>
        let _, smallestGapBetweenRings =
            bigToSmall
            |> Seq.fold (fun (previousRadius, soFar) thisRing ->
                let gap = previousRadius - thisRing.Radius
                let newSmallest =
                    if gap < minGapForLabel then soFar
                    else min soFar gap
                (thisRing.Radius, newSmallest)) target.PaperSize
        [|  for i, ring in bigToSmall |> Seq.indexed do
                let smallerRingRadius =
                    if i < bigToSmall.Length - 1 then Some bigToSmall.[i+1].Radius else None
                let textElem (labelPart : string) (x : string) (y : string) =
                    XElement(xn "text"
                        , attr "id" ("label-" + labelPart + "-" + ring.Label)
                        , attr "x" x
                        , attr "y" y
                        , attr "text-anchor" "middle"
                        , attr "dominant-baseline" "central"
                        , attr "stroke" "none"
                        , attr "fill" (color ring.Fill.ContrastColor)
                        , attr "font-size" (mm (0.5 * smallestGapBetweenRings))
                        , attr "font-family" "sans-serif"
                        , ring.Label
                        )
                match smallerRingRadius with
                | None ->
                    if target.LabelClockPositions.Length > 0 && ring.Radius >= minGapForLabel then
                        yield textElem "x" (mm centerX) (mm centerY)
                | Some smallerRingRadius ->
                    if ring.Radius - smallerRingRadius >= minGapForLabel then
                        let middleRadius = smallerRingRadius + 0.5 * (ring.Radius - smallerRingRadius)
                        for clock in defaultArg ring.LabelClockPositionsOverride target.LabelClockPositions do
                            let (x, y) = offsetClock (centerX, centerY) clock middleRadius
                            yield textElem (string clock) (mm x) (mm y)
        |]
    let svgElement =
        XElement(xn "svg"
            , attr "version" "1.1"
            , attr "width" (mm (fst target.PaperSize) + "mm")
            , attr "height" (mm (snd target.PaperSize) + "mm")
            , attr "viewBox" ("0 0 " + mm (fst target.PaperSize) + " " + mm (snd target.PaperSize))
            , XElement(xn "g"
                , attr "id" "border"
                , XElement(xn "rect"
                    , attr "x" "0"
                    , attr "y" "0"
                    , attr "width" (mm (fst target.PaperSize))
                    , attr "height" (mm (snd target.PaperSize))
                    , attr "stroke" (color (if config.DrawBorder then Black else White))
                    , attr "stroke-width" (mm config.RingThickness)
                    , attr "fill" (color White)
                    )
                )
            , XElement(xn "g", [| yield attr "id" "rings" :> obj; for ring in rings do yield upcast ring |])
            , XElement(xn "g", [| yield attr "id" "ring-labels" :> obj; for label in labels do yield upcast label |])
            , if config.IncludeInfo then info else null
            , if config.IncludeRuler then ruler else null
            )
    XDocument(svgElement)