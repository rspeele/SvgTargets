open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open SvgTargets

let private inch (x : float) = x * (1.0<m> / 39.3701)
let private foot (x : float) = inch (x * 12.0)
let private yard (x : float) = foot (x * 3.0)
let private mm (x: float) = x * (1.0<m> / 1000.0)

[<EntryPoint>]
let main argv =
    for target in Targets.NRA.allTargets do
        let config = { Svg.SvgConfiguration.Default with RingThickness = if target.Distance > 20.0<m> then 0.0008<m> else 0.0005<m> }
        let rendered = Svg.renderTarget config target
        rendered.Save(target.Identifier + ".svg")

        let scaled5y = Scaling.scale (inch 0.22) (inch 0.177) (yard 5.0) target
        (Svg.renderTarget { config with RingThickness = 0.0002<m> } scaled5y).Save(scaled5y.Identifier + "-5y-air.svg")
    0 // return an integer exit code