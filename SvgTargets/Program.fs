open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open SvgTargets

[<EntryPoint>]
let main argv =
    for target in Targets.NRA.allTargets do
        let config = { Svg.SvgConfiguration.Default with RingThickness = if target.Distance > 20.0<m> then 0.0008<m> else 0.0005<m> }
        let rendered = Svg.renderTarget config target
        rendered.Save(target.Identifier + ".svg")
    0 // return an integer exit code