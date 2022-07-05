#r "nuget:FeatherDotNet"
#r "nuget: NumSharp"
#r "./bin/Debug/net6.0/Cortado.FSharp.dll"
#nowarn "9"

open System
open System.Collections.Generic
open NumSharp
open Cortado.FSharp
open System.Linq
open Microsoft.FSharp.NativeInterop

let df = DataFrame.FromFeather("airlinetrain.feather")
let label = df.GetFactor("dep_delayed_15min").AsCovariate(fun level -> if level = "Y" then 1.0f else 0.0f).AsCached()
let depTime = df.GetCovariate("DepTime").AsFactor().AsCached()
let distance = df.GetCovariate("Distance").AsFactor().AsCached()
//let factors = df.Factors |> Array.filter (fun f -> f.Name <> "dep_delayed_15min")
//                                    |> Array.append [|depTime|]
let mu = 0.5f
let eta = 0.1f
let nrounds = 100
let maxDepth = 6
let lambda = 1.0f
let gamma = 0.0f
let minh = 1.0f
let slicelen = 100000
let factors = [df.GetFactor("Month");df.GetFactor("DayofMonth");df.GetFactor("DayOfWeek");
                             df.GetFactor("UniqueCarrier");
                             df.GetFactor("Origin");df.GetFactor("Dest");depTime;distance]
//let factors = [depTime]
let (tree, p) = Logistic.xgblogit label factors mu eta lambda gamma maxDepth nrounds minh slicelen
let psum = p |> Array.map float |> Array.sum
p

let getGH (trees : XGTree list, treeIndex: int, layerIndex : int) =
    let (XGTree layers) = trees[treeIndex]
    let (TreeLayer nodes) = layers[layerIndex]
    nodes |> List.iter (fun n ->
        n.Partitions |> Seq.filter (fun kv -> kv.Value |> Array.reduce (&&) = false)
            |> Seq.iter (fun kv -> printfn "Split index: %A" (kv.Value))
        printfn "G: %f H: %f Loss: %f" n.GSum n.HSum n.Loss
    )

//getGH(tree, 0, 1)

