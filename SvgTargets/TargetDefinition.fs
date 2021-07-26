namespace SvgTargets
open FSharp.Data.UnitSystems.SI.UnitSymbols

type RingFill =
    | White
    | Black
    member this.ContrastColor =
        match this with
        | White -> Black
        | Black -> White

type RingDefinition =
    {   Radius : float<m>
        Label : string
        Fill : RingFill
        LabelClockPositionsOverride : float array option
    }

type TargetDefinition =
    {   Organization : string // e.g. "NRA"
        Identifier : string // e.g. "B-6"
        Name : string // e.g. "50 yard slow fire pistol target"
        Rings : RingDefinition array
        PaperSize : float<m> * float<m> // w, h
        Distance : float<m>
        LabelClockPositions : float array
    }

module Targets =
    let private inch (x : float) = x * (1.0<m> / 39.3701)
    let private foot (x : float) = inch (x * 12.0)
    let private yard (x : float) = foot (x * 3.0)
    let private mm (x: float) = x * (1.0<m> / 1000.0)
    
    let private ring (label : string) (radius : float<m>) =
        {   Label = label
            Radius = radius
            Fill = White
            LabelClockPositionsOverride = None
        }

    let private black (ring : RingDefinition) = { ring with Fill = Black }

    module NRA =
        let private nra = "NRA"
        let private diameter x = inch (x * 0.5)

        let b2 =
            {   Organization = nra
                Identifier = "B-2"
                Name = "50-foot slow fire pistol target"
                Rings =
                    [|  ring "10" (diameter 0.90) |> black
                        ring "9" (diameter 1.54) |> black
                        ring "8" (diameter 2.23) |> black
                        ring "7" (diameter 3.07) |> black
                        ring "6" (diameter 4.16)
                        ring "5" (diameter 5.56)
                        ring "4" (diameter 7.33)
                    |]
                PaperSize = (inch 8.5, inch 10.0)
                Distance = foot 50.0
                LabelClockPositions = [| 9.0 |]
            }

        let b3 =
            {   Organization = nra
                Identifier = "B-3"
                Name = "50-foot timed and rapid fire pistol target"
                Rings =
                    [|  ring "X" (diameter 0.90) |> black
                        ring "10" (diameter 1.80) |> black
                        ring "9" (diameter 3.06) |> black
                        ring "8" (diameter 4.46)
                        ring "7" (diameter 6.14)
                        ring "6" (diameter 8.32)
                    |]
                PaperSize = (inch 8.5, inch 10.0)
                Distance = foot 50.0
                LabelClockPositions = [| 9.0 |]
            }

        let b4 =
            {   Organization = nra
                Identifier = "B-4"
                Name = "20-yard slow fire pistol target"
                Rings =
                    [|  ring "10" (diameter 1.12) |> black
                        ring "9" (diameter 1.88) |> black
                        ring "8" (diameter 2.72) |> black
                        ring "7" (diameter 3.73) |> black
                        ring "6" (diameter 5.04)
                        ring "5" (diameter 6.72)
                        ring "4" (diameter 8.84)
                    |]
                PaperSize = (inch 10.5, inch 12.0)
                Distance = yard 20.0
                LabelClockPositions = [| 9.0 |]
            }

        let b5 =
            {   Organization = nra
                Identifier = "B-5"
                Name = "20-yard timed and rapid fire pistol target"
                Rings =
                    [|  ring "X" (diameter 1.12) |> black
                        ring "10" (diameter 2.25) |> black
                        ring "9" (diameter 3.76) |> black
                        ring "8" (diameter 5.44)
                        ring "7" (diameter 7.46)
                        ring "6" (diameter 10.08)
                    |]
                PaperSize = (inch 10.5, inch 12.0)
                Distance = yard 20.0
                LabelClockPositions = [| 9.0 |]
            }

        let b16 =
            {   Organization = nra
                Identifier = "B-16"
                Name = "25-yard slow fire pistol target"
                Rings =
                    [|  ring "X" (diameter 0.67) |> black
                        ring "10" (diameter 1.51) |> black
                        ring "9" (diameter 2.60) |> black
                        ring "8" (diameter 3.82) |> black
                        ring "7" (diameter 5.32) |> black
                        ring "6" (diameter 7.22)
                        ring "5" (diameter 9.66)
                    |]
                PaperSize = (inch 10.5, inch 12.0)
                Distance = yard 25.0
                LabelClockPositions = [| 9.0 |]
            }

        let b6 =
            {   Organization = nra
                Identifier = "B-6"
                Name = "50-yard slow fire pistol target"
                Rings =
                    [|  ring "X" (diameter 1.695) |> black
                        ring "10" (diameter 3.36) |> black
                        ring "9" (diameter 5.54) |> black
                        ring "8" (diameter 8.00) |> black
                        ring "7" (diameter 11.00)
                        ring "6" (diameter 14.80)
                        ring "5" (diameter 19.68)
                    |]
                PaperSize = (inch 21.0, inch 24.0)
                Distance = yard 50.0
                LabelClockPositions = [| 3.0; 9.0 |]
            }

        let b8 =
            {   Organization = nra
                Identifier = "B-8"
                Name = "25-yard timed and rapid fire pistol target"
                Rings = b6.Rings |> Array.map (fun r -> if r.Label = "8" then { r with Fill = White } else r)
                PaperSize = (inch 21.0, inch 24.0)
                Distance = yard 25.0
                LabelClockPositions = [| 3.0; 9.0 |]
            }

        let b40 =
            let mmdi x = mm (x * 0.5)
            {   Organization = nra
                Identifier = "B-40"
                Name = "International 10 meter air pistol target"
                Rings =
                    [|  ring "X" (mmdi 5.0) |> black
                        ring "10" (mmdi 11.5) |> black
                        ring "9" (mmdi 27.5) |> black
                        ring "8" (mmdi 43.5) |> black
                        ring "7" (mmdi 59.5) |> black
                        ring "6" (mmdi 75.5)
                        ring "5" (mmdi 91.5)
                        ring "4" (mmdi 107.5)
                        ring "3" (mmdi 123.5)
                        ring "2" (mmdi 139.5)
                        ring "1" (mmdi 155.5)
                    |]
                PaperSize = (inch 7.0, inch 8.0)
                Distance = yard 11.0
                LabelClockPositions = [| 12.0; 3.0; 6.0; 9.0 |]
            }

        let allTargets = [| b2; b3; b4; b5; b6; b8; b16; b40 |]

    let allTargets = NRA.allTargets
