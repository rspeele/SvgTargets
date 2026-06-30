module SvgTargets.Svg
open FSharp.Data.UnitSystems.SI.UnitSymbols
open System.Xml
open System.Xml.Linq
open System

type GridConfiguration =
    {   SpacingX : float<m>
        SpacingY : float<m>
        Color : string option
    }

type SvgConfiguration =
    {   IncludeRuler : bool
        IncludeInfo : bool
        IncludeGrid : GridConfiguration option
        RingThickness : float<m>
        BlackOverride : string option
        WhiteOverride : string option
        DrawBorder : bool
    }
    static member Default =
        {   IncludeRuler = false
            IncludeInfo = true
            IncludeGrid = None
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
    let grid =
        match config.IncludeGrid with
        | None -> null
        | Some grid ->
        let width, height = target.PaperSize
        let line x1 x2 y1 y2 =
            XElement(xn "line", attr "x1" (mm x1), attr "x2" (mm x2), attr "y1" (mm y1), attr "y2" (mm y2)
                , attr "stroke" (defaultArg grid.Color "red")
                , attr "stroke-width" (mm config.RingThickness)
                )
        let hlines =
            let hline y = line 0.0<m> width y y
            [|  for y in centerY .. grid.SpacingY .. height do yield hline y
                for y in seq { centerY .. -grid.SpacingY .. 0.0<m> } |> Seq.skip 1 do yield hline y
            |]
        let vlines =
            let vline x = line x x 0.0<m> height
            [|  for x in centerX .. grid.SpacingX .. width do yield vline x
                for x in seq { centerX .. -grid.SpacingX .. 0.0<m> } |> Seq.skip 1 do yield vline x
            |]
        XElement(xn "g", hlines, vlines)
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
        let minGapForLabel = 0.002<m>
        let _, smallestGapBetweenRings =
            bigToSmall
            |> Seq.fold (fun (previousRadius, soFar) thisRing ->
                let gap = previousRadius - thisRing.Radius
                let newSmallest =
                    if gap < minGapForLabel || thisRing.LabelClockPositionsOverride = Some [||] then soFar
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
                let clocks = defaultArg ring.LabelClockPositionsOverride target.LabelClockPositions
                match smallerRingRadius with
                | None ->
                    if clocks.Length > 0 && ring.Radius >= minGapForLabel then
                        yield textElem "x" (mm centerX) (mm centerY)
                | Some smallerRingRadius ->
                    if ring.Radius - smallerRingRadius >= minGapForLabel then
                        let middleRadius = smallerRingRadius + 0.5 * (ring.Radius - smallerRingRadius)
                        for clock in clocks do
                            let (x, y) = offsetClock (centerX, centerY) clock middleRadius
                            yield textElem (string clock) (mm x) (mm y)
        |]
    let decorations =
        [|  for d in target.Decorations do
                match d with
                | Line (x1, y1, x2, y2, thickness, fill) ->
                    yield XElement(xn "line"
                        , attr "x1" (mm (centerX + x1))
                        , attr "y1" (mm (centerY + y1))
                        , attr "x2" (mm (centerX + x2))
                        , attr "y2" (mm (centerY + y2))
                        , attr "stroke" (color fill)
                        , attr "stroke-width" (mm thickness)
                        , attr "stroke-linecap" "butt"
                        )
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
            , XElement(xn "g", [| yield attr "id" "decorations" :> obj; for d in decorations do yield upcast d |])
            , if config.IncludeInfo then info else null
            , if config.IncludeRuler then ruler else null
            , grid
            )
    XDocument(svgElement)