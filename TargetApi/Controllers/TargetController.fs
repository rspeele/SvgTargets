namespace TargetApi.Controllers
open System
open System.IO
open Microsoft.AspNetCore.Mvc
open FSharp.Data.UnitSystems.SI.UnitSymbols
open SvgTargets
open SvgTargets.Svg
open SvgTargets.Conversions

[<ApiController>]
type TargetController () =
    inherit ControllerBase()

    [<HttpGet>]
    [<Route("/{organization}/{identifier}.svg")>]
    member this.Get
        ( organization : string
        , identifier : string
        , scaleDistance : string
        , scaleOldCaliber : Nullable<float>
        , scaleNewCaliber : Nullable<float>
        , ringThickness : Nullable<float>
        , black : string
        , white : string
        , gridColor : string
        , info : Nullable<int>
        , dime : Nullable<int>
        , border : Nullable<int>
        , paperWidth : Nullable<float>
        , paperHeight : Nullable<float>
        , gridX : Nullable<float>
        , gridY : Nullable<float>
        ) =
        let target =
            Targets.allTargets
            |> Seq.tryFind (fun t -> 
                t.Organization.Equals(organization, StringComparison.OrdinalIgnoreCase)
                && t.Identifier.Equals(identifier, StringComparison.OrdinalIgnoreCase))
        match target with
        | None ->
            NotFoundResult() :> IActionResult
        | Some target ->
            let target =
                if isNull scaleDistance then target else
                let scaleDistance =
                    if scaleDistance.EndsWith('y') then yard (float (scaleDistance.Substring(0, scaleDistance.Length - 1)))
                    else if scaleDistance.EndsWith('m') then 1.0<m> * float (scaleDistance.Substring(0, scaleDistance.Length - 1))
                    else if scaleDistance.EndsWith('f') then foot (float (scaleDistance.Substring(0, scaleDistance.Length - 1)))
                    else 1.0<m> * float scaleDistance
                let scaleOldCaliber = if scaleOldCaliber.HasValue then inch scaleOldCaliber.Value else inch 0.22
                let scaleNewCaliber = if scaleNewCaliber.HasValue then inch scaleNewCaliber.Value else inch 0.22
                Scaling.scale scaleOldCaliber scaleNewCaliber scaleDistance target

            let target =
                let w, h = target.PaperSize
                { target with
                   PaperSize = (if paperWidth.HasValue then inch paperWidth.Value else w), (if paperHeight.HasValue then inch paperHeight.Value else h)
                }

            let grid =
                if not gridX.HasValue && not gridY.HasValue then None
                else
                    {   SpacingX = if gridX.HasValue then inch gridX.Value else inch gridY.Value
                        SpacingY = if gridY.HasValue then inch gridY.Value else inch gridX.Value
                        Color = if isNull gridColor then None else Some gridColor
                    } |> Some
            let config =
                {   IncludeRuler = dime <> Nullable(0)
                    IncludeInfo = info <> Nullable(0)
                    IncludeGrid = grid
                    RingThickness = if ringThickness.HasValue then ringThickness.Value / inchesPerMeter else (min 25.0<m> target.Distance) * 0.00002
                    BlackOverride = if isNull black then None else Some black
                    WhiteOverride = if isNull white then None else Some white
                    DrawBorder = border = Nullable(1)
                }
            let rendered = renderTarget config target

            use mem = new MemoryStream()
            rendered.Save(mem)
            this.File(mem.ToArray(), "image/svg+xml") :> IActionResult

    // Two-level catalog (organization + discipline, then targets) consumed by the
    // interactive builder page (builder.html) so its dropdowns stay in sync with
    // the code.
    [<HttpGet>]
    [<Route("/targets.json")>]
    member this.List() : IActionResult =
        let categories =
            Targets.categories
            |> Array.map (fun (org, (discipline : Discipline), targets) ->
                {|  category = org + " " + discipline.Name
                    organization = org
                    discipline = discipline.Name
                    targets =
                        targets
                        |> Array.map (fun t ->
                            let w, h = t.PaperSize
                            {|  identifier = t.Identifier
                                name = t.Name
                                distanceYards = t.Distance * yardsPerMeter
                                paperWidthInches = w * inchesPerMeter
                                paperHeightInches = h * inchesPerMeter
                            |})
                |})
        this.Ok(categories) :> IActionResult

