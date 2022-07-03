namespace Cortado.FSharp

open System
open System.Collections.Generic
open SeqUtils

module XGSplit =

    let getloss (g: float) (h: float) (lambda: float32) =
        -(g * g) / (h + float lambda)

    let f_splitnode (issplitnode: bool[]) (leftpartitions: bool[]) (maxlevelcount: int) (factorindex: int[]) (nodemap: int[]) (nodeIdSlice: Memory<int>, fslices: Memory<int>) =
        let slicelen = nodeIdSlice.Length
        let nodeIdSliceSpan = nodeIdSlice.Span
        let fslicesSpan = fslices.Span
        for i in 0..slicelen - 1 do
            let nodeid = nodeIdSliceSpan[i]
            if issplitnode[nodeid] then
                let levelindex = fslicesSpan[factorindex[nodeid] * slicelen + i]
                nodeIdSliceSpan[i] <-
                    if leftpartitions[nodeid * maxlevelcount + levelindex] then
                        nodemap[nodeIdSliceSpan[i]]
                    else
                        nodemap[nodeIdSliceSpan[i]] + 1
            else
                nodeIdSliceSpan[i] <- nodemap[nodeIdSliceSpan[i]]

    let f_hist (acc0: float[]* float[]*bool[]*int) (zipslice: Memory<int> * Memory<int> * Memory<float32> * Memory<float32>) =
        let nodeslice, factorslice, gslice, hslice = zipslice
        let gsum, hsum, nodecansplit, levelCount = acc0
        let nodesliceSpan = nodeslice.Span
        let factorSliceSpan = factorslice.Span
        let gsliceSpan = gslice.Span
        let hsliceSpan = hslice.Span
        for i in 0..nodeslice.Length - 1 do
            let nodeid = nodesliceSpan[i]
            if nodecansplit[nodeid] then
                let levelindex = factorSliceSpan[i]
                gsum[nodeid * levelCount + levelindex] <- float(gsliceSpan[i]) + gsum[nodeid * levelCount + levelindex]
                hsum[nodeid * levelCount + levelindex] <- float(hsliceSpan[i]) + hsum[nodeid * levelCount + levelindex]
        (gsum, hsum, nodecansplit, levelCount)

    let get_hist_slice (gsum0) (hsum0) (nodeids: int[]) (nodecansplit) (factor: Factor) (gcovariate: Covariate) (hcovariate: Covariate)
                       (start) (length) (slicelen) =

        let nodeIdsSlicer = VectorSlicer(nodeids.AsMemory()):>ISliceProvider<int>
        let levelCount = factor.Levels.Length
        let nodeslices = nodeIdsSlicer.GetSlices(start, length, slicelen)
        let factorslices = factor.SliceProvider.GetSlices(start, length, slicelen)
        let gslices = gcovariate.SliceProvider.GetSlices(start, length, slicelen)
        let hslices = hcovariate.SliceProvider.GetSlices(start, length, slicelen)
        let zipslices = SeqUtils.zip4 nodeslices factorslices gslices hslices
        zipslices |> Seq.fold f_hist (gsum0, hsum0, nodecansplit, levelCount)

    let get_histogram (nodeIds: int[]) (nodecansplit: bool[]) (factor: Factor) (gcovariate: Covariate)
                      (hcovariate: Covariate) (slicelen: int) =

        let nodecount = nodecansplit.Length
        let levelcount = factor.Levels.Length
        let start = 0
        let length = nodeIds.Length

        let gsum = Array.zeroCreate<float> (nodecount * levelcount)
        let hsum = Array.zeroCreate<float> (nodecount * levelcount)

        let _  = get_hist_slice gsum hsum nodeIds nodecansplit factor gcovariate hcovariate start length slicelen
        (gsum, hsum)

    let get_best_stump_split (g_hist: Memory<float>) (h_hist: Memory<float>) (partition: bool[]) (lambda: float32) (minh: float32) =
        let gsum = g_hist.ToArray() |> Array.sum
        let hsum = h_hist.ToArray() |> Array.sum
        let ghistSpan = g_hist.Span
        let hhistSpan = h_hist.Span
        let currloss = getloss gsum hsum lambda
        let mutable bestloss = currloss
        let mutable stump_index = -1
        let minh = float minh

        for i in 0..partition.Length - 1 do
            if partition[i] then
                let loss = (getloss ghistSpan[i] hhistSpan[i] lambda) + getloss (gsum - ghistSpan[i]) (hsum - hhistSpan[i]) lambda
                if loss < bestloss && (hhistSpan[i] >= minh) && (hsum - hhistSpan[i] >= minh) then
                    bestloss <- loss
                    stump_index <- i

        if stump_index >= 0 then
            let leftpartition = partition |> Array.copy
            let rightpartition = partition |> Array.copy
            for i in 0..partition.Length - 1 do
                if partition[i] then
                    if i = stump_index then
                        rightpartition[i] <- false
                    else
                        leftpartition[i] <- false
            let leftgsum = ghistSpan[stump_index]
            let lefthsum = hhistSpan[stump_index]
            let rightgsum = gsum - leftgsum
            let righthsum = hsum - lefthsum
            (currloss, bestloss, leftpartition, rightpartition, leftgsum, lefthsum, rightgsum, righthsum)
        else
            (currloss, bestloss, partition, partition, 0.0, 0.0, 0.0, 0.0)

    let get_splitnode (factor: Factor) (leafnode: LeafInfo) (histogram: Memory<float> * Memory<float>) (lambda: float32) (minh: float32) =

        let g_hist, h_hist = histogram
        let partition = leafnode.Partitions[factor]

        let currloss, bestloss, leftpartition, rightpartition, leftgsum, lefthsum, rightgsum, righthsum =
            if factor.IsOrdinal then
                //currloss, bestloss, leftpartition, rightpartition, leftgsum, lefthsum, rightgsum, righthsum = get_best_range_split(g_hist, h_hist, partition, lambda_, minh)
                get_best_stump_split g_hist h_hist partition lambda minh
            else
                get_best_stump_split g_hist h_hist partition lambda minh

        if bestloss < currloss then
            let leftpartitions = Dictionary(leafnode.Partitions)
            let rightpartitions = Dictionary(leafnode.Partitions)
            leftpartitions[factor] <- leftpartition
            rightpartitions[factor] <- rightpartition
            let leftcansplit = true //any([np.sum(v) > 1 for k, v in leftpartitions.items()])
            let rightcansplit = true // any([np.sum(v) > 1 for k, v in rightpartitions.items()])
            let leftloss = getloss leftgsum lefthsum lambda
            let rightloss = getloss rightgsum righthsum lambda
            let leftnode = {GSum=leftgsum; HSum = lefthsum; CanSplit = leftcansplit; Partitions = leftpartitions; Loss = leftloss}
            let rightnode = {GSum=rightgsum; HSum = righthsum; CanSplit = rightcansplit; Partitions = rightpartitions; Loss = rightloss}
            Split({Factor = factor; LeftLeaf = leftnode; RightLeaf = rightnode; Loss = bestloss; Gain = currloss - bestloss})
        else
            Leaf(leafnode)

    let getnewsplit (histograms: float[] * float[]) (nodes: LeafInfo list) (factor: Factor) (lambda: float32) (gamma: float32) (minh: float32) =
        let levelCount = factor.Levels.Length
        let g, h = histograms
        nodes |> List.mapi (fun i node ->
            let hist = g.AsMemory().Slice(i * levelCount, levelCount), h.AsMemory().Slice(i * levelCount, levelCount)
            get_splitnode factor node hist lambda minh
        )

    let findbestsplit (state: TreeGrowState) =
        let (XGTree layers) = state.XGTree
        let (TreeLayer lastLayer) = layers |> List.last

        let nodecansplit = lastLayer |> List.map (fun x -> x.CanSplit) |> List.toArray

        let mingain = state.Gamma |> float

        let f (currSplit: DecisionNode list) (factor: Factor) =
            let histograms = get_histogram state.NodeIds nodecansplit factor state.GCovariate state.HCovariate state.SliceLength

            let newsplit = getnewsplit histograms lastLayer factor state.Lambda state.Gamma state.MinH
            currSplit |> List.zip newsplit |> List.map (fun (newNode, currNode) ->
                                                       match newNode with
                                                           | Split(s) ->
                                                               let newLoss = s.Loss
                                                               let newGain = s.Gain
                                                               if newGain > mingain && newLoss < currNode.Loss then
                                                                   newNode
                                                               else
                                                                   currNode
                                                            | _ -> currNode
                                                   )

        let decisionNodes0 = lastLayer |> List.map Leaf
        state.Factors |> Array.fold f decisionNodes0

    let f_weights (nodeIds: int[]) (weights: float32[]) (fm: float32[]) (eta: float32) =
        for i in 0..nodeIds.Length - 1 do
            fm[i] <- eta * weights[nodeIds[i]] + fm[i]

    let splitnodeidsslice (nodeIds: int[]) (factors: Factor[]) (issplitnode: bool[]) (nodemap: int[]) (leftpartitions: bool[]) (maxlevelcount: int) (factorindex: int[])
                          (start: int) (length: int) (slicelength: int) =
        if factors.Length > 0 then
            let factorslices = factors |> Array.map (fun f -> f.SliceProvider.GetSlices(start, length,slicelength))
                                                            |> SeqUtils.zipNMem
            let nodeslices = (VectorSlicer(nodeIds):>ISliceProvider<int>).GetSlices(start, length, slicelength)
            factorslices |> Seq.zip nodeslices |> Seq.iter (f_splitnode issplitnode leftpartitions maxlevelcount factorindex nodemap)

    let splitnodeids (nodeIds: int[]) (nodes: DecisionNode list) (slicelength: int) =
        let nodecount = nodes.Length
        let length = nodeIds.Length
        let start = 0
        let issplitnode = nodes |> List.map (fun n -> match n with | Split(_) -> true | _ -> false)

        let (nodemap', _, _) = issplitnode |> List.fold (fun (nodemap: ResizeArray<int>, i:int, splitnodecount: int) isSplit ->
                                                                                nodemap.Add(i + splitnodecount)
                                                                                let splitnodecount' = if isSplit then splitnodecount + 1 else splitnodecount
                                                                                (nodemap, i + 1, splitnodecount')
                                                                                ) (ResizeArray<int>(), 0, 0)


        let nodemap = nodemap'.ToArray()
        let issplitnode = issplitnode |> List.toArray

        let factors = ResizeArray<Factor>()
        let factorindex = Array.zeroCreate<int> nodecount
        for i in 0..nodecount - 1 do
            match nodes[i] with
                | Split(s) ->
                    let factor = s.Factor
                    let index = factors.IndexOf(factor)
                    if index = -1 then
                        factors.Add(factor)
                        factorindex[i] <- factors.Count - 1
                    else
                        factorindex[i] <- index
                | Leaf(l) ->
                    factorindex[i] <- -1

        let maxlevelcount = nodes |> List.map (fun v -> match v with | Split(s) -> s.Factor.Levels.Length | _ -> 0)
                                             |> List.max

        let leftpartitions = Array.zeroCreate<bool> (nodecount * maxlevelcount)
        nodes |> List.iteri (fun i n ->
            match n with
                | Split(s) ->
                    let leftpart = s.LeftLeaf.Partitions[s.Factor]
                    leftpart.AsMemory().CopyTo(leftpartitions.AsMemory().Slice(i * maxlevelcount))
                | _ -> ()
        )
        let factors = factors.ToArray()
        splitnodeidsslice nodeIds factors issplitnode nodemap leftpartitions maxlevelcount factorindex start length slicelength
        nodeIds

    let updatestate (state: TreeGrowState) (layernodes: DecisionNode list) =
        let newNodeIds = splitnodeids state.NodeIds layernodes state.SliceLength
        let newLayer = layernodes |> List.collect (fun node ->
                                  match node with
                                      | Split(n) -> [n.LeftLeaf; n.RightLeaf]
                                      | Leaf(n) -> [n]
                                  )
        let (XGTree layers) = state.XGTree
        {state with NodeIds = newNodeIds; XGTree = XGTree(layers @ [TreeLayer(newLayer)])}

    let nextLayer (state: TreeGrowState) (step: int) =
        let layerNodes = findbestsplit state
        updatestate state layerNodes

    let predict (nodes: LeafInfo list) (nodeIds: int[]) (fm: float32[]) (eta: float32) (lambda) =
        let nodes = nodes |> List.toArray
        let weights = Array.zeroCreate<float32>(nodes.Length)
        for i in 0..nodes.Length - 1 do
            let node = nodes[i]
            weights[i] <- -node.GSum / (node.HSum + lambda) |> float32

        f_weights nodeIds weights fm eta

    let growTree (factors) (gcovariate: Covariate) (hcovariate: Covariate) (fm: float32[]) (eta: float32) (maxDepth: int)
                  (lambda) (gamma) (minh) (slicelen) =

        let length = gcovariate.Length
        let nodeIds = Array.zeroCreate<int> length

        let loss0 = Double.MaxValue
        let nodes0 =  [ { GSum = 0.0; HSum = 0.0; CanSplit = true;
                                          Partitions = Dictionary([for f: Factor in factors -> KeyValuePair(f, Array.create f.Levels.Length true)]);
                                          Loss = loss0} ]

        let state0 =
            {
                NodeIds = nodeIds
                XGTree = XGTree([TreeLayer(nodes0)])
                Factors = factors
                GCovariate = gcovariate
                HCovariate = hcovariate
                Gamma = gamma
                Lambda = lambda
                MinH = minh
                SliceLength = slicelen
            }
        let res = {1..maxDepth} |> Seq.fold nextLayer state0
        let xgTree = res.XGTree
        let (XGTree layers) = xgTree
        let (TreeLayer lastLayer) = layers |> List.last
        predict lastLayer res.NodeIds fm eta (float lambda)
        xgTree, fm
