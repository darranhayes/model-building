#r "nuget: Flips"
open Flips
open Flips.Types
open Flips.SliceMap

type Product = Product of string
type ProcessType =
    | Grinding
    | Drilling

let products =
    [
        Product "PROD1"
        Product "PROD2"
        Product "PROD3"
        Product "PROD4"
        Product "PROD5"
    ]

let productProfit =
    [
        Product "PROD1", 550.0
        Product "PROD2", 600.0
        Product "PROD3", 350.0
        Product "PROD4", 400.0
        Product "PROD5", 200.0
    ] |> SMap

let productDuration =
    [
        ((Product "PROD1", Grinding), 12.0)
        ((Product "PROD1", Drilling), 10.0)
        ((Product "PROD2", Grinding), 20.0)
        ((Product "PROD2", Drilling),  8.0)
        ((Product "PROD3", Grinding),  0.0)
        ((Product "PROD3", Drilling), 16.0)
        ((Product "PROD4", Grinding), 25.0)
        ((Product "PROD4", Drilling),  0.0)
        ((Product "PROD5", Grinding), 15.0)
        ((Product "PROD6", Drilling),  0.0)
    ] |> SMap2

let unitCapacity = products |> List.map (fun product -> product, 20.0) |> SMap

let productDecisions =
    DecisionBuilder "ProductDecisions" {
        for product in products -> Continuous (0.0, infinity)
    } |> SMap

let grinders = 3
let drillers = 2
let hoursPerShift = 8
let shiftsPerDay = 2
let daysPerWeek = 6
let staff = 8

let grindingCapacityConstraint =
    Constraint.create "GrindingCapacity" (sum (productDecisions .* productDuration.[All, Grinding]) <== float (grinders * hoursPerShift * shiftsPerDay * daysPerWeek))

let drillingCapacityConstraint =
    Constraint.create "DrillingCapacity" (sum (productDecisions .* productDuration.[All, Drilling]) <== float (drillers * hoursPerShift * shiftsPerDay * daysPerWeek))

let labourConstraint =
    Constraint.create "LabourCapcity" (sum (productDecisions .* unitCapacity) <== float (staff * hoursPerShift * daysPerWeek))

let profitExpr = sum (productDecisions .* productProfit)

let objective = Objective.create "MaximiseProfit" Maximize profitExpr

let model =
    Model.create objective
    |> Model.addConstraint grindingCapacityConstraint
    |> Model.addConstraint drillingCapacityConstraint
    |> Model.addConstraint labourConstraint

let settings = {
    SolverType = SolverType.CBC
    MaxDuration = 10_000L
    WriteLPFile = None
    WriteMPSFile = None
}

// Solve the model and save the result
let result = Solver.solve settings model

printfn "--Results--"
// Print the results
match result with
| Optimal solution ->
    printfn "Objective Value: %f" (Objective.evaluate solution objective)
    for (decision, value) in solution.DecisionResults |> Map.toSeq do
        let (DecisionName name) = decision.Name
        printfn "Decision: %s\tValue: %f" name value
| _ -> printfn "Unable to solve."