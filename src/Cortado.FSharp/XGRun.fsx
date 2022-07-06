#r "nuget:FeatherDotNet"
#r "./bin/Release/net6.0/Cortado.FSharp.dll"
#nowarn "9"

open System
open Cortado.FSharp

let df = DataFrame.FromFeather("airlinetrain.feather")
let label = df.GetFactor("dep_delayed_15min").AsCovariate(fun level -> if level = "Y" then 1.0f else 0.0f).AsCached()
let depTime = df.GetCovariate("DepTime").AsFactor().AsCached()
let distance = df.GetCovariate("Distance").AsFactor().AsCached()
let factors = df.Factors |> Array.filter (fun f -> f.Name <> "dep_delayed_15min")
                                    |> Array.append [|depTime;distance|]
let mu = 0.5f
let eta = 0.1f
let nrounds = 100
let maxDepth = 6
let lambda = 1.0f
let gamma = 0.0f
let minh = 1.0f
let slicelen = 100000

#time
let (tree, p) = Logistic.xgblogit label factors mu eta lambda gamma maxDepth nrounds minh slicelen
#time
let auc = Logistic.auc p label
let psum = p |> Array.map float |> Array.sum



