#r "nuget: Flips"
open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure

[<Measure>] type Ton
[<Measure>] type GBP
[<Measure>] type Hardness

type Product = Product of string

type ProductDefinition =
    {
        Name : Product
        Profit : float<GBP/Ton>
        VegetableOil : float
        NonVegetableOil : float
        Continuity : float
        UpperHardness : float<Hardness>
        LowerHardness : float<Hardness>
    }

let minWeight = 0.0<Ton>
let maxWeight = 1_000_000.0<Ton>
let vegetableOilRefiningCapacity = 200.0<Ton>
let nonVegetableOilRefiningCapacity = 250.0<Ton>

let products : ProductDefinition list =
    [
        { Name = Product "VEG1"; Profit = -110.0<GBP/Ton>; VegetableOil = 1.0; NonVegetableOil = 0.0; Continuity = 1.0;  UpperHardness = 8.8<Hardness>;  LowerHardness = 8.8<Hardness>; }
        { Name = Product "VEG2"; Profit = -120.0<GBP/Ton>; VegetableOil = 1.0; NonVegetableOil = 0.0; Continuity = 1.0;  UpperHardness = 6.1<Hardness>;  LowerHardness = 6.1<Hardness>; }
        { Name = Product "OIL1"; Profit = -130.0<GBP/Ton>; VegetableOil = 0.0; NonVegetableOil = 1.0; Continuity = 1.0;  UpperHardness = 2.0<Hardness>;  LowerHardness = 2.0<Hardness>; }
        { Name = Product "OIL2"; Profit = -110.0<GBP/Ton>; VegetableOil = 0.0; NonVegetableOil = 1.0; Continuity = 1.0;  UpperHardness = 4.2<Hardness>;  LowerHardness = 4.2<Hardness>; }
        { Name = Product "OIL3"; Profit = -115.0<GBP/Ton>; VegetableOil = 0.0; NonVegetableOil = 1.0; Continuity = 1.0;  UpperHardness = 5.0<Hardness>;  LowerHardness = 5.0<Hardness>; }
        { Name = Product "PROD"; Profit = 150.0<GBP/Ton>;  VegetableOil = 0.0; NonVegetableOil = 0.0; Continuity = -1.0; UpperHardness = -6.0<Hardness>; LowerHardness = -3.0<Hardness>; }
    ]

let profit =
    products
    |> List.map (fun record -> (record.Name, record.Profit))
    |> SMap

let vegetableOilRefinementFactor =
    products
    |> List.map (fun record -> (record.Name, record.VegetableOil))
    |> SMap

let nonVegetableOilRefinementFactor =
    products
    |> List.map (fun record -> (record.Name, record.NonVegetableOil))
    |> SMap

let continuity =
    products
    |> List.map (fun record -> (record.Name, record.Continuity))
    |> SMap

let upperHardness =
    products
    |> List.map (fun record -> (record.Name, record.UpperHardness))
    |> SMap

let lowerHardness =
    products
    |> List.map (fun record -> (record.Name, record.LowerHardness))
    |> SMap

let productDecisions =
    let products =
        products
        |> List.map (fun record -> record.Name)
    DecisionBuilder<Ton> "ProductDecisions" {
        for product in products -> Continuous (minWeight, maxWeight)
    } |> SMap

let objective =
    Objective.create "MaximiseProfit" Maximize (sum (productDecisions .* profit))

let vegetableOilRefiningCapacityConstraint =
    Constraint.create "VegetableOilRefiningCapacity" (sum (productDecisions .* vegetableOilRefinementFactor) <== vegetableOilRefiningCapacity)

let nonVegetableOilRefiningCapacityConstraint =
    Constraint.create "NonVegetableOilRefiningCapacity" (sum (productDecisions .* nonVegetableOilRefinementFactor) <== nonVegetableOilRefiningCapacity)

let hardnessUpperBoundConstraint =
    Constraint.create "UpperBoundHardness" (sum (productDecisions .* upperHardness) <== 0.0<Hardness Ton>)

let hardnessLowerBoundConstraint =
    Constraint.create "LowerBoundHardness" (sum (productDecisions .* lowerHardness) >== 0.0<Hardness Ton>)

let productRefinementEqualityConstraint =
    Constraint.create "ProductRefinementEquality" (sum (productDecisions .* continuity) == 0.0<Ton>)

let model =
    Model.create objective
    |> Model.addConstraint vegetableOilRefiningCapacityConstraint
    |> Model.addConstraint nonVegetableOilRefiningCapacityConstraint
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
