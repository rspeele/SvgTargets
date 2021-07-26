module SvgTargets.Conversions
open FSharp.Data.UnitSystems.SI.UnitSymbols

let inchesPerMeter = 39.3701<1/m>
let feetPerMeter = inchesPerMeter / 12.0
let yardsPerMeter = feetPerMeter / 3.0 

let inch (x : float) = x / inchesPerMeter
let foot (x : float) = x / feetPerMeter
let yard (x : float) = x / yardsPerMeter

let mm (x: float) = x * (1.0<m> / 1000.0)

