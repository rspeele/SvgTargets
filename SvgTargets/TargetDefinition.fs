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
                    [|  ring "X" (inchdi 0.67) |> black |> noLabel
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

        let a17 =
            {   Organization = nra
                Identifier = "A-17_1"
                Name = "50-foot smallbore rifle target (single-bull)"
                Rings =
                    [|  ring "10" (inchdi 0.150) |> black
                        ring "9" (inchdi 0.483) |> black
                        ring "8" (inchdi 0.817) |> black
                        ring "7" (inchdi 1.150) |> black
                        ring "6" (inchdi 1.483) |> black
                        ring "5" (inchdi 1.817)
                    |]
                PaperSize = (inch 4.0, inch 4.0)
                Distance = foot 50.0
                LabelClockPositions = [| |]
            }

        let usas50 =
            {   Organization = nra
                Identifier = "USAS-50_1"
                Name = "50m int'l target reduced to 50 feet (single-bull)"
                Rings =
                    [|  ring "10" (mmdi 0.76) |> black
                        ring "9" (mmdi 4.12) |> black
                        ring "8" (mmdi 9.00) |> black
                        ring "7" (mmdi 13.87) |> black
                        ring "6" (mmdi 18.75) |> black
                        ring "5" (mmdi 23.63) |> black
                        ring "4" (mmdi 28.50) |> black
                        ring "3" (mmdi 33.38) |> black
                    |]
                PaperSize = (inch 4.0, inch 4.0)
                Distance = foot 50.0
                LabelClockPositions = [| |]
            }

        let a32 =
            {   Organization = nra
                Identifier = "A-32_1"
                Name = "50-foot NRA Light Rifle target (single-bull)"
                Rings =
                    [|  ring "X" (inchdi 0.218)
                        ring "10" (inchdi 0.439) |> black
                        ring "9" (inchdi 1.187) |> black
                        ring "8" (inchdi 1.874) |> black
                        ring "7" (inchdi 2.656)
                        ring "6" (inchdi 3.374)
                    |]
                PaperSize = (inch 4.0, inch 5.0)
                Distance = foot 50.0
                LabelClockPositions = [| |]
            }

        let a7 =
            {   Organization = nra
                Identifier = "A-7_1"
                Name = "75-foot smallbore rifle target (single-bull)"
                Rings =
                    [|  ring "10" (inchdi 0.335) |> black
                        ring "9" (inchdi 0.835) |> black
                        ring "8" (inchdi 1.335) |> black
                        ring "7" (inchdi 1.835) |> black
                        ring "6" (inchdi 2.335) |> black
                        ring "5" (inchdi 2.835)
                    |]
                PaperSize = (inch 4.0, inch 5.0)
                Distance = foot 75.0
                LabelClockPositions = [| |]
            }

        let a23 =
            {   Organization = nra
                Identifier = "A-23_1"
                Name = "50-yard smallbore rifle target (single-bull)"
                Rings =
                    [|  ring "X" (inchdi 0.39) |> black |> noLabel
                        ring "10" (inchdi 0.89) |> black |> noLabel
                        ring "9" (inchdi 1.89) |> black
                        ring "8" (inchdi 2.89) |> black
                        ring "7" (inchdi 3.89) |> black
                        ring "6" (inchdi 4.89)
                        ring "5" (inchdi 5.89)
                    |]
                PaperSize = (inch 6.0, inch 7.0)
                Distance = yard 50.0
                LabelClockPositions = [| 9.0 |]
            }

        let a27 =
            {   Organization = nra
                Identifier = "A-27_1"
                Name = "50-yard smallbore prone target (single-bull)"
                Rings =
                    [|  ring "X" (inchdi 0.359) |> black |> noLabel
                        ring "10" (inchdi 0.719) |> black |> noLabel
                        ring "9" (inchdi 1.439) |> black
                        ring "8" (inchdi 2.159) |> black
                        ring "7" (inchdi 2.879) |> black
                        ring "6" (inchdi 3.599) |> black
                        ring "black" (inchdi 3.89) |> black |> noLabel
                        ring "5" (inchdi 4.319)
                        ring "4" (inchdi 5.038)
                    |]
                PaperSize = (inch 6.0, inch 7.0)
                Distance = yard 50.0
                LabelClockPositions = [| 9.0 |]
            }

        let a51 =
            {   Organization = nra
                Identifier = "A-51_1"
                Name = "50-yard reduction of international 50m rifle target (single-bull)"
                Rings =
                    [|  ring "X" (mmdi 4.096) |> black |> noLabel
                        ring "10" (mmdi 9.034) |> black |> noLabel
                        ring "9" (mmdi 23.664) |> black |> noLabel
                        ring "8" (mmdi 38.295) |> black
                        ring "7" (mmdi 52.925) |> black
                        ring "6" (mmdi 67.556) |> black
                        ring "5" (mmdi 82.186) |> black
                        ring "4" (mmdi 96.816) |> black
                        ring "black" (mmdi 102.78) |> black |> noLabel
                        ring "3" (mmdi 111.447)
                        ring "2" (mmdi 126.077)
                        ring "1" (mmdi 140.708)
                    |]
                PaperSize = (mm 180.0, mm 180.0)
                Distance = yard 50.0
                LabelClockPositions = [| 12.0; 3.0; 6.0; 9.0 |]
            }

        let a26 =
            {   Organization = nra
                Identifier = "A-26_1"
                Name = "50-meter smallbore prone target (single-bull)"
                Rings =
                    [|  ring "X" (inchdi 0.393) |> black
                        ring "10" (inchdi 0.787) |> black
                        ring "9" (inchdi 1.574) |> black
                        ring "8" (inchdi 2.361) |> black
                        ring "7" (inchdi 3.148) |> black
                        ring "6" (inchdi 3.936) |> black
                        ring "black" (inchdi 4.270) |> black |> noLabel
                        ring "5" (inchdi 4.723)
                        ring "4" (inchdi 5.510)
                    |]
                PaperSize = (inch 6.0, inch 7.0)
                Distance = 50.0<m>
                LabelClockPositions = [| 9.0 |]
            }

        let a50 =
            {   Organization = nra
                Identifier = "A-50_1"
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

        let a25 =
            {   Organization = nra
                Identifier = "A-25_1"
                Name = "100-yard smallbore prone target (single-bull)"
                Rings =
                    [|  ring "X" (inchdi 1.0) |> black
                        ring "10" (inchdi 2.0) |> black
                        ring "9" (inchdi 4.0) |> black
                        ring "8" (inchdi 6.0) |> black
                        ring "7" (inchdi 8.0) |> black
                        ring "6" (inchdi 10.0)
                        ring "5" (inchdi 12.0)
                    |]
                PaperSize = (inch 14.0, inch 14.0)
                Distance = yard 100.0
                LabelClockPositions = [| 9.0 |]
            }

        let a33 =
            {   Organization = nra
                Identifier = "A-33_1"
                Name = "100-yard reduction of international 300m target for Metric Prone competition (single-bull)"
                Rings =
                    [|  ring "10" (inchdi 1.045) |> black
                        ring "9" (inchdi 2.245) |> black
                        ring "8" (inchdi 3.445) |> black
                        ring "7" (inchdi 4.645) |> black
                        ring "6" (inchdi 5.845) |> black
                        ring "5" (inchdi 7.045) |> black
                        ring "4" (inchdi 8.245) |> black
                        ring "3" (inchdi 9.445)
                        ring "2" (inchdi 10.645)
                        ring "1" (inchdi 11.845)
                    |]
                PaperSize = (inch 14.0, inch 14.0)
                Distance = yard 100.0
                LabelClockPositions = [| 9.0 |]
            }

        let a21 =
            {   Organization = nra
                Identifier = "A-21"
                Name = "200-yard smallbore target"
                Rings =
                    [|  ring "X" (inchdi 2.0) |> black
                        ring "10" (inchdi 4.0) |> black
                        ring "9" (inchdi 8.0) |> black
                        ring "8" (inchdi 12.0) |> black
                        ring "7" (inchdi 16.0)
                        ring "6" (inchdi 20.0)
                    |]
                PaperSize = (inch 22.0, inch 24.0)
                Distance = yard 200.0
                LabelClockPositions = [| 9.0; 3.0 |]
            }

        let a31 =
            {   Organization = nra
                Identifier = "A-31_1"
                Name = "50-yard NRA Light Rifle target (single-bull)"
                Rings =
                    [|  ring "10" (inchdi 1.025) |> black
                        ring "9" (inchdi 2.21) |> black
                        ring "8" (inchdi 3.42) |> black
                        ring "7" (inchdi 4.165) |> black
                        ring "6" (inchdi 5.812)
                    |]
                PaperSize = (inch 6.0, inch 8.0)
                Distance = yard 50.0
                LabelClockPositions = [| 9.0 |]
            }

        let a37 =
            {   Organization = nra
                Identifier = "A-37_1"
                Name = "100-yard mini-Palma smallbore prone reduction of 800y LR target"
                Rings =
                    [|  ring "X" (inchdi 1.0) |> black
                        ring "10" (inchdi 2.0) |> black
                        ring "9" (inchdi 2.9) |> black
                        ring "8" (inchdi 3.9) |> black
                        ring "7" (inchdi 4.9) |> black
                        ring "6" (inchdi 5.8) |> black
                        ring "5" (inchdi 6.8)
                    |]
                PaperSize = (inch 8.0, inch 8.0)
                Distance = yard 100.0
                LabelClockPositions = [| 3.0; 9.0 |]
            }

        let a38 =
            {   Organization = nra
                Identifier = "A-38_1"
                Name = "100-yard mini-Palma smallbore prone reduction of 900y LR target"
                Rings =
                    [|  ring "X" (inchdi 0.9) |> black
                        ring "10" (inchdi 1.8) |> black
                        ring "9" (inchdi 2.6) |> black
                        ring "8" (inchdi 3.4) |> black
                        ring "7" (inchdi 4.3) |> black
                        ring "6" (inchdi 5.1) |> black
                        ring "5" (inchdi 6.0)
                    |]
                PaperSize = (inch 8.0, inch 8.0)
                Distance = yard 100.0
                LabelClockPositions = [| 3.0; 9.0 |]
            }

        let a39 =
            {   Organization = nra
                Identifier = "A-39_1"
                Name = "100-yard mini-Palma smallbore prone reduction of 1000y LR target"
                Rings =
                    [|  ring "X" (inchdi 0.8) |> black
                        ring "10" (inchdi 1.6) |> black
                        ring "9" (inchdi 2.3) |> black
                        ring "8" (inchdi 3.1) |> black
                        ring "7" (inchdi 3.8) |> black
                        ring "6" (inchdi 4.6) |> black
                        ring "5" (inchdi 5.4)
                    |]
                PaperSize = (inch 8.0, inch 8.0)
                Distance = yard 100.0
                LabelClockPositions = [| 3.0; 9.0 |]
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

        let sr3 =
            {   Organization = nra
                Identifier = "SR-3"
                Name = "Enlarged aiming black version of SR target, for 300-yard rapid fire matches"
                Rings =
                    [|  ring "X" (inchdi 3.0) |> black
                        { (ring "10" (inchdi 7.0) |> black) with LabelClockPositionsOverride = Some [|12.0|] }
                        { (ring "9" (inchdi 13.0) |> black) with LabelClockPositionsOverride = Some [|12.0|] }
                        { (ring "8" (inchdi 19.0) |> black) with LabelClockPositionsOverride = Some [|12.0|] }
                        { (ring "7" (inchdi 25.0)) with LabelClockPositionsOverride = Some [|12.0|] }
                        ring "6" (inchdi 31.0)
                        ring "5" (inchdi 37.0)
                    |]
                PaperSize = (inch 40.0, inch 40.0)
                Distance = yard 300.0
                LabelClockPositions = [| 3.0; 9.0 |]
            }

        let mr63 =
            {   Organization = nra
                Identifier = "MR-63"
                Name = "300-yard reduction of MR-1 600y target"
                Rings =
                    [|  ring "X" (inchdi 2.85) |> black
                        ring "10" (inchdi 5.85) |> black
                        ring "9" (inchdi 8.85) |> black
                        ring "8" (inchdi 11.85) |> black
                        ring "7" (inchdi 17.85) |> black
                        ring "6" (inchdi 23.85)
                        ring "5" (inchdi 29.85)
                    |]
                PaperSize = (inch 40.0, inch 40.0)
                Distance = yard 300.0
                LabelClockPositions = [| 3.0 |]
            }

        let mr65 =
            {   Organization = nra
                Identifier = "MR-65"
                Name = "Rifle, Competition Mid-Range. Used in 500-yard matches only"
                Rings =
                    [|  ring "X" (inchdi 5.0) |> black
                        ring "10" (inchdi 10.0) |> black
                        ring "9" (inchdi 15.0) |> black
                        ring "8" (inchdi 20.0) |> black
                        ring "7" (inchdi 25.0) |> black
                        ring "6" (inchdi 30.0) |> black
                        ring "5" (inchdi 36.0)
                    |]
                PaperSize = (inch 40.0, inch 40.0)
                Distance = yard 500.0
                LabelClockPositions = [| 12.0 |]
            }

        let mr1 =
            {   Organization = nra
                Identifier = "MR-1"
                Name = "Enlarged aiming black for use in 600-yard matches only."
                Rings =
                    [|  ring "X" (inchdi 6.0) |> black
                        ring "10" (inchdi 12.0) |> black
                        ring "9" (inchdi 18.0) |> black
                        ring "8" (inchdi 24.0) |> black
                        ring "7" (inchdi 36.0) |> black
                        ring "6" (inchdi 48.0)
                        ring "5" (inchdi 60.0)
                    |]
                PaperSize = (inch 64.0, inch 64.0)
                Distance = yard 600.0
                LabelClockPositions = [| 12.0 |]
            }

        let lr =
            {   Organization = nra
                Identifier = "LR"
                Name = "Long range target."
                Rings =
                    [|  ring "X" (inchdi 10.0) |> black
                        ring "10" (inchdi 20.0) |> black
                        ring "9" (inchdi 30.0) |> black
                        ring "8" (inchdi 44.0) |> black
                        ring "7" (inchdi 60.0)
                    |]
                PaperSize = (inch 72.0, inch 72.0)
                Distance = yard 1000.0
                LabelClockPositions = [| 12.0 |]
            }

        let allTargets =
            [|  b2; b3; b4; b5; b6; b8; b16; b40

                a17; usas50; a32; a7; a23; a27
                a51; a26; a50; a25; a33; a21; a31
                a37; a38; a39

                sr1; sr21; mr31
                sr; sr42; mr52; sr5
                sr3; mr63
                mr65
                mr1
                lr
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
                Distance = 50.0<m>
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
