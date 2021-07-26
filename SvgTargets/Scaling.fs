module SvgTargets.Scaling
open System
open SvgTargets.Conversions
open FSharp.Data.UnitSystems.SI.UnitSymbols

let private addRadiusToRings (radiusIncrease : float<m>) (target : TargetDefinition) =
    { target with
        Rings = target.Rings |> Array.map (fun r -> { r with Radius = r.Radius + radiusIncrease })
    }

let private scaleRings (radiusMultiplier : float) (target : TargetDefinition) =
    { target with
        Rings = target.Rings |> Array.map (fun r -> { r with Radius = r.Radius * radiusMultiplier })
    }

let private describeDistance (distance : float<m>) =
    let units =
        [   (distance * yardsPerMeter, "y")
            (distance * feetPerMeter, "'")
            (distance * 1.0<1/m>, "m")
        ]
    let closestToWholeNumber, units =
        units |> List.sortBy (fun (dist, _) -> abs (dist - round dist)) |> List.head
    sprintf "%g%s" closestToWholeNumber units

let scale (originalCaliber : float<m>) (newCaliber : float<m>) (newDistance : float<m>) (target : TargetDefinition) =
    let originalLaserTarget = addRadiusToRings (originalCaliber * 0.5) target
    let distanceRatio = newDistance / target.Distance
    let scaledLaserTarget = scaleRings distanceRatio originalLaserTarget
    let newShootableTarget = addRadiusToRings (-newCaliber * 0.5) scaledLaserTarget

    let radiusOfOldBull= target.Rings |> Seq.filter (fun r -> r.Fill = Black) |> Seq.map (fun r -> r.Radius) |> Seq.max
    // shooter's eye should be about 1m behind their muzzle and the firing line, so include that extra distance in the ratio
    let visualDistanceRatio = (newDistance + 1.0<m>) / (target.Distance + 1.0<m>)
    let radiusOfNewBull = radiusOfOldBull * visualDistanceRatio
    let nearestRingToRadius = newShootableTarget.Rings |> Seq.minBy (fun r -> abs (r.Radius - radiusOfNewBull))
    let oldW, oldH = target.PaperSize
    { newShootableTarget with
        Distance = newDistance
        Rings =
            newShootableTarget.Rings
            |> Seq.map (fun r -> { r with Radius = max 0.0<m> r.Radius; Fill = if r.Radius <= nearestRingToRadius.Radius then Black else White })
            |> Seq.toArray
        PaperSize = oldW * visualDistanceRatio, oldH * visualDistanceRatio
        Name = newShootableTarget.Name + " (scaled to " + describeDistance newDistance + ")"
    }
    

