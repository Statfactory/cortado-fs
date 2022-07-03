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
let covariates = df.Covariates
let dep_delayed_15min = df.GetFactor("dep_delayed_15min")
let cov = dep_delayed_15min.AsCovariate(fun level -> if level = "Y" then 1.0f else 0.0f)
//let dist = df.GetCovariate("DepTime")
let label = cov.AsCached()
let factors = df.Factors |> Array.filter (fun f -> f.Name = "Month")

let mu = 0.5f
let eta = 0.1f
let nrounds = 10
let maxDepth = 6
let lambda = 1.0f
let gamma = 0.0f
let minh = 1.0f
let slicelen = 100000
let res = Logistic.xgblogit label factors mu eta lambda gamma maxDepth nrounds minh slicelen
let p = res |> snd
