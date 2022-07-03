namespace Cortado.FSharp

open System
open System.Collections.Generic

type LeafInfo =
    {
     GSum: float
     HSum: float
     CanSplit: bool
     Partitions: Dictionary<Factor, bool[]>
     Loss: float
    }

type SplitInfo =
    {
     Factor: Factor
     LeftLeaf: LeafInfo
     RightLeaf: LeafInfo
     Loss: float
     Gain: float
    }

type DecisionNode =
    | Leaf of LeafInfo
    | Split of SplitInfo
    member this.Loss =
        match this with
            | Leaf(x) -> x.Loss
            | Split(x) -> x.Loss

type TreeLayer = TreeLayer of LeafInfo list

type XGTree = XGTree of TreeLayer list

type TreeGrowState =
    {
        NodeIds : int[]
        XGTree : XGTree
        Factors : Factor[]
        GCovariate : Covariate
        HCovariate : Covariate
        Gamma : float32
        Lambda : float32
        MinH : float32
        SliceLength : int
    }
