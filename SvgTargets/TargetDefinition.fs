namespace SvgTargets
open FSharp.Data.UnitSystems.SI.UnitSymbols
open SvgTargets.Conversions

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
    
    let private ring (label : string) (radius : float<m>) =
        {   Label = label
            Radius = radius
            Fill = White
            LabelClockPositionsOverride = None
        }

    let private black (ring : RingDefinition) = { ring with Fill = Black }
    let private noLabel (ring : RingDefinition) = { ring with LabelClockPositionsOverride = Some [||] }

    let private mmdi (x : float) = mm (x * 0.5)
    let private inchdi (x : float) = inch (x * 0.5)

    module NRA =
        let private nra = "NRA"

        let b2 =
            {   Organization = nra
                Identifier = "B-2"
                Name = "50-foot slow fire pistol target"
                Rings =
                    [|  ring "10" (inchdi 0.90) |> black
                        ring "9" (inchdi 1.54) |> black
                        ring "8" (inchdi 2.23) |> black
                        ring "7" (inchdi 3.07) |> black
                        ring "6" (inchdi 4.16)
                        ring "5" (inchdi 5.56)
                        ring "4" (inchdi 7.33)
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
                    [|  ring "X" (inchdi 0.90) |> black
                        ring "10" (inchdi 1.80) |> black
                        ring "9" (inchdi 3.06) |> black
                        ring "8" (inchdi 4.46)
                        ring "7" (inchdi 6.14)
                        ring "6" (inchdi 8.32)
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
                    [|  ring "10" (inchdi 1.12) |> black
                        ring "9" (inchdi 1.88) |> black
                        ring "8" (inchdi 2.72) |> black
                        ring "7" (inchdi 3.73) |> black
                        ring "6" (inchdi 5.04)
                        ring "5" (inchdi 6.72)
                        ring "4" (inchdi 8.84)
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
                    [|  ring "X" (inchdi 1.12) |> black
                        ring "10" (inchdi 2.25) |> black
                        ring "9" (inchdi 3.76) |> black
                        ring "8" (inchdi 5.44)
                        ring "7" (inchdi 7.46)
                        ring "6" (inchdi 10.08)
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
                    [|  ring "X" (inchdi 0.67) |> black
                        ring "10" (inchdi 1.51) |> black
                        ring "9" (inchdi 2.60) |> black
                        ring "8" (inchdi 3.82) |> black
                        ring "7" (inchdi 5.32) |> black
                        ring "6" (inchdi 7.22)
                        ring "5" (inchdi 9.66)
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
                    [|  ring "X" (inchdi 1.695) |> black
                        ring "10" (inchdi 3.36) |> black
                        ring "9" (inchdi 5.54) |> black
                        ring "8" (inchdi 8.00) |> black
                        ring "7" (inchdi 11.00)
                        ring "6" (inchdi 14.80)
                        ring "5" (inchdi 19.68)
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
            {   Organization = nra
                Identifier = "B-40"
                Name = "International 10 meter air pistol target"
                Rings =
                    [|  ring "X" (mmdi 5.0) |> black |> noLabel
                        ring "10" (mmdi 11.5) |> black |> noLabel
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

        let sr1 =
            {   Organization = nra
                Identifier = "SR-1"
                Name = "100-yard reduction of SR 200y target"
                Rings =
                    [|  ring "X" (inchdi 1.35) |> black
                        ring "10" (inchdi 3.35) |> black
                        ring "9" (inchdi 6.35) |> black
                        ring "8" (inchdi 9.35)
                        ring "7" (inchdi 12.35)
                        ring "6" (inchdi 15.35)
                        ring "5" (inchdi 18.35)
                    |]
                PaperSize = (inch 20.0, inch 20.0)
                Distance = yard 100.0
                LabelClockPositions = [| 3.0 |]
            }

        let sr21 =
            {   Organization = nra
                Identifier = "SR-21"
                Name = "100-yard reduction of SR-3 300y target"
                Rings =
                    [|  ring "X" (inchdi 0.79) |> black
                        ring "10" (inchdi 2.12) |> black
                        ring "9" (inchdi 4.12) |> black
                        ring "8" (inchdi 6.12) |> black
                        ring "7" (inchdi 8.12)
                        ring "6" (inchdi 10.12)
                        ring "5" (inchdi 12.12)
                    |]
                PaperSize = (inch 20.0, inch 20.0)
                Distance = yard 100.0
                LabelClockPositions = [| 3.0 |]
            }

        let mr31 =
            {   Organization = nra
                Identifier = "MR-31"
                Name = "100-yard reduction of MR-1 600y target"
                Rings =
                    [|  ring "X" (inchdi 0.75) |> black
                        ring "10" (inchdi 1.75) |> black
                        ring "9" (inchdi 2.75) |> black
                        ring "8" (inchdi 3.75) |> black
                        ring "7" (inchdi 5.75) |> black
                        ring "6" (inchdi 7.75)
                        ring "5" (inchdi 9.75)
                    |]
                PaperSize = (inch 20.0, inch 20.0)
                Distance = yard 100.0
                LabelClockPositions = [| 3.0 |]
            }

        let sr =
            {   Organization = nra
                Identifier = "SR"
                Name = "Target, Rifle, Competition, Short Range"
                Rings =
                    [|  ring "X" (inchdi 3.0) |> black
                        { (ring "10" (inchdi 7.0) |> black) with LabelClockPositionsOverride = Some [|12.0|] }
                        { (ring "9" (inchdi 13.0) |> black) with LabelClockPositionsOverride = Some [|12.0|] }
                        ring "8" (inchdi 19.0)
                        ring "7" (inchdi 25.0)
                        ring "6" (inchdi 31.0)
                        ring "5" (inchdi 37.0)
                    |]
                PaperSize = (inch 40.0, inch 40.0)
                Distance = yard 200.0
                LabelClockPositions = [| 3.0; 9.0 |]
            }

        let sr42 =
            {   Organization = nra
                Identifier = "SR-42"
                Name = "200-yard reduction of SR-3 300y target"
                Rings =
                    [|  ring "X" (inchdi 1.9) |> black
                        ring "10" (inchdi 4.56) |> black
                        ring "9" (inchdi 8.56) |> black
                        ring "8" (inchdi 12.56) |> black
                        ring "7" (inchdi 16.56)
                        ring "6" (inchdi 20.56)
                        ring "5" (inchdi 24.56)
                    |]
                PaperSize = (inch 28.0, inch 28.0)
                Distance = yard 200.0
                LabelClockPositions = [| 3.0; 9.0 |]
            }

        let mr52 =
            {   Organization = nra
                Identifier = "MR-52"
                Name = "200-yard reduction of MR-1 600y target"
                Rings =
                    [|  ring "X" (inchdi 1.79) |> black
                        ring "10" (inchdi 3.79) |> black
                        ring "9" (inchdi 5.79) |> black
                        ring "8" (inchdi 7.79) |> black
                        ring "7" (inchdi 11.79) |> black
                        ring "6" (inchdi 15.79)
                        ring "5" (inchdi 19.79)
                    |]
                PaperSize = (inch 28.0, inch 28.0)
                Distance = yard 200.0
                LabelClockPositions = [| 3.0 |]
            }

        let sr5 =
            { sr with
                Identifier = "SR-5"
                Name = "Modified SR target for small target frames"
                Rings = sr.Rings.[0..4]
                PaperSize = (inch 28.0, inch 28.0)
            }

        let allTargets =
            [|  b2; b3; b4; b5; b6; b8; b16; b40
                sr1; sr21; mr31
                sr; sr42; mr52; sr5
            |]

    module ISSF =
        let private issf = "ISSF"

        let r300 =
            {   Organization = issf
                Identifier = "R300"
                Name = "300m Rifle Target"
                Rings =
                    [|  ring "+" (mmdi 50.0) |> black
                        ring "10" (mmdi 100.0) |> black |> noLabel
                        ring "9" (mmdi 200.0) |> black
                        ring "8" (mmdi 300.0) |> black
                        ring "7" (mmdi 400.0) |> black
                        ring "6" (mmdi 500.0) |> black
                        ring "5" (mmdi 600.0) |> black
                        ring "4" (mmdi 700.0)
                        ring "3" (mmdi 800.0)
                        ring "2" (mmdi 900.0)
                        ring "1" (mmdi 1000.0)
                    |]
                PaperSize = (mm 1050.0, mm 1050.0)
                Distance = 300.0<m>
                LabelClockPositions = [| 1.5; 4.5; 7.5; 10.5 |]
            }

        let r50 =
            {   Organization = issf
                Identifier = "R50"
                Name = "50m Rifle Target"
                Rings =
                    [|  ring "X" (mmdi 5.0) |> black |> noLabel
                        ring "10" (mmdi 10.4) |> black |> noLabel
                        ring "9" (mmdi 26.4) |> black |> noLabel
                        ring "8" (mmdi 42.4) |> black
                        ring "7" (mmdi 58.4) |> black
                        ring "6" (mmdi 74.4) |> black
                        ring "5" (mmdi 90.4) |> black
                        ring "4" (mmdi 106.4) |> black
                        ring "black" (mmdi 112.4) |> black |> noLabel
                        ring "3" (mmdi 122.4)
                        ring "2" (mmdi 138.4)
                        ring "1" (mmdi 154.4)
                    |]
                PaperSize = (mm 180.0, mm 180.0)
                Distance = 50.0<m>
                LabelClockPositions = [| 12.0; 3.0; 6.0; 9.0 |]
            }

        let ar10 =
            {   Organization = issf
                Identifier = "AR10"
                Name = "10m Air Rifle Target"
                Rings =
                    [|  ring "10" (mmdi 0.5) |> black |> noLabel
                        ring "9" (mmdi 5.5) |> black |> noLabel
                        ring "8" (mmdi 10.5) |> black
                        ring "7" (mmdi 15.5) |> black
                        ring "6" (mmdi 20.5) |> black
                        ring "5" (mmdi 25.5) |> black
                        ring "4" (mmdi 30.5) |> black
                        ring "3" (mmdi 35.5)
                        ring "2" (mmdi 40.5)
                        ring "1" (mmdi 45.5)
                    |]
                PaperSize = (mm 100.0, mm 100.0)
                Distance = 10.0<m>
                LabelClockPositions = [| 12.0; 3.0; 6.0; 9.0 |]
            }

        let p25 =
            {   Organization = issf
                Identifier = "P25"
                Name = "25m Precision Pistol Target"
                Rings =
                    [|  ring "X" (mmdi 25.0) |> black |> noLabel
                        ring "10" (mmdi 50.0) |> black |> noLabel
                        ring "9" (mmdi 100.0) |> black
                        ring "8" (mmdi 150.0) |> black
                        ring "7" (mmdi 200.0) |> black
                        ring "6" (mmdi 250.0)
                        ring "5" (mmdi 300.0)
                        ring "4" (mmdi 350.0)
                        ring "3" (mmdi 400.0)
                        ring "2" (mmdi 450.0)
                        ring "1" (mmdi 500.0)
                    |]
                PaperSize = (mm 550.0, mm 550.0)
                Distance = 25.0<m>
                LabelClockPositions = [| 12.0; 3.0; 6.0; 9.0 |]
            }

        let p50 =
            { p25 with
                Identifier = "P50"
                Name = "50m Precision Pistol Target"
            }

        let ap10 =
            { NRA.b40 with
                Identifier = "AP10"
                Organization = issf
                Name = "10m Air Pistol Target"
            }

        let allTargets =
            [|  r300
                r50
                ar10
                p25
                p50
                ap10
            |]

    let allTargets = Array.concat [| NRA.allTargets; ISSF.allTargets |]
