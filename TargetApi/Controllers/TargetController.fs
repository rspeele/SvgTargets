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
        , info : Nullable<int>
        , dime : Nullable<int>
        , border : Nullable<int>
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

            let config =
                {   IncludeRuler = dime <> Nullable(0)
                    IncludeInfo = info <> Nullable(0)
                    RingThickness = if ringThickness.HasValue then ringThickness.Value / inchesPerMeter else (min 25.0<m> target.Distance) * 0.00002
                    BlackOverride = if isNull black then None else Some black
                    WhiteOverride = if isNull white then None else Some white
                    DrawBorder = border = Nullable(1)
                }
            let rendered = renderTarget config target

            use mem = new MemoryStream()
            rendered.Save(mem)
            this.File(mem.ToArray(), "image/svg+xml") :> IActionResult
            
