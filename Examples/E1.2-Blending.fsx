#r "nuget: Flips"
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

[<Measure>]
type Ton

[<Measure>]
type GBP

[<Measure>]
type Hard

type Product = Product of string

let minWeight = 0.0<Ton>
let maxWeight = 1_000_000.0<Ton>
let salePrice = 150.0<GBP/Ton>

let products =
    [
        Product "VEG1"
        Product "VEG2"
        Product "OIL1"
        Product "OIL2"
        Product "OIL3"
    ]

let productCosts =
    [
        Product "VEG1", 110.0<GBP/Ton>
        Product "VEG2", 120.0<GBP/Ton>
        Product "OIL1", 130.0<GBP/Ton>
        Product "OIL2", 110.0<GBP/Ton>
        Product "OIL3", 115.0<GBP/Ton>
    ] |> SMap

let productHardness =
    [
        Product "VEG1", 8.8<Hard>
        Product "VEG2", 6.1<Hard>
        Product "OIL1", 2.0<Hard>
        Product "OIL2", 4.2<Hard>
        Product "OIL3", 5.0<Hard>
    ] |> SMap

let productDecisions =
    DecisionBuilder<Ton> "ProductDecisions" {
        for product in products -> Continuous (minWeight, maxWeight)
    } |> SMap

let amountOfProductToProduce = Decision.createContinuous<Ton> "AmountOfProductToProduce" minWeight maxWeight

let salesExpr = amountOfProductToProduce * salePrice
let costsExpr = sum (productDecisions .* productCosts)
let profitExpr = salesExpr - costsExpr

let objective =
    Objective.create "MaximiseProfit" Maximize profitExpr

let vegRefiningCapacityConstraint =
    Constraint.create "VegRefiningCapacity" (sum (productDecisions.[ Between (Product "VEG1", Product "VEG2") ]) <== 200.0<Ton>)

let oilRefiningCapacityConstraint =
    Constraint.create "OilRefiningCapacity" (sum (productDecisions.[ Between (Product "OIL1", Product "OIL3") ]) <== 250.0<Ton>)

let hardnessUpperBoundConstraint =
    Constraint.create "UpperBoundHardness" (sum (productDecisions .* productHardness) <== amountOfProductToProduce * 6.0<Hard>)

let hardnessLowerBoundConstraint =
    Constraint.create "LowerBoundHardness" (sum (productDecisions .* productHardness) >== amountOfProductToProduce * 3.0<Hard>)

let productRefinementEqualityConstraint =
    Constraint.create "ProductRefinementEquality" ((sum productDecisions - amountOfProductToProduce) == 0.0<Ton>)

let model =
    Model.create objective
    |> Model.addConstraint vegRefiningCapacityConstraint
    |> Model.addConstraint oilRefiningCapacityConstraint
    |> Model.addConstraint hardnessUpperBoundConstraint
    |> Model.addConstraint hardnessLowerBoundConstraint
    |> Model.addConstraint productRefinementEqualityConstraint

let settings = {
    SolverType = SolverType.CBC
    MaxDuration = 10_000L
    WriteLPFile = None
    WriteMPSFile = None
}

let result = Solver.solve settings model

match result with
| Optimal solution ->
    let value = Objective.evaluate solution objective
    printfn "Objective Value: %f GBP" value
    for (decision, value) in solution.DecisionResults |> Map.toSeq do
        let (DecisionName name) = decision.Name
        printfn "Decision: %s\tValue: %f Ton" name value
| _ -> printfn "Unable to solve."
