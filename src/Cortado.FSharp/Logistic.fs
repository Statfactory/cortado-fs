namespace Cortado.FSharp

open System
open Literals
open XGSplit

module Logistic =

    let eps = Single.Epsilon

    let inline logitG (yhat:float32) (y: float32) =
        yhat - y

    let inline logitH(yhat: float32) =
        max (yhat * (1.0f - yhat)) eps

    let inline logitRaw(p: float32) =
        -Math.Log(float(1.0f / p - 1.0f)) |> float32

    let inline sigmoid(x: float32) =
        1.0f / (1.0f + float32(Math.Exp(-float(x))))

    let inline getGH (yhatraw: float32[]) (label: Covariate) (g: float32[]) (h: float32[]) (sliceLen: int) =
        let n = g.Length
        let slices = label.SliceProvider.GetSlices(0, n, sliceLen)
        slices |> Seq.fold (fun offset slice ->
            let sliceSpan = slice.Span
            let yhatrawSpan = yhatraw.AsMemory().Span
            let gSpan = g.AsMemory().Span
            let hSpan = h.AsMemory().Span
            for i in 0..slice.Length - 1 do
                let yhat = sigmoid(yhatrawSpan[offset + i])
                let y = sliceSpan[i]
                gSpan[offset + i] <- logitG yhat y
                hSpan[offset + i] <- logitH yhat
            offset + slice.Length
        ) 0 |> ignore
        ()

    let inline getPred (fm: float32[]) =
        let fm = fm.AsMemory().Span
        for i in 0..fm.Length - 1 do
            fm[i] <- sigmoid(fm[i])

    let xgblogit (label: Covariate) (factors: seq<Factor>) (mu: float32)
                   (eta: float32) (lambda: float32) (gamma: float32)
                   (maxDepth: int) (nrounds: int) (minh: float32) (slicelen: int) =

        let n = label.Length
        let factors = factors |> Seq.toArray
        let f0 = Array.create n (logitRaw(mu))
        let g = Array.zeroCreate<float32> n
        let h = Array.zeroCreate<float32> n

        let step (acc) (m) =
            let fm, trees = acc

            getGH fm label g h slicelen
            let g_cov = Covariate("g", g)
            let h_cov = Covariate("h", h)
            let tree, predraw = growTree factors g_cov h_cov fm eta maxDepth lambda gamma minh slicelen
            predraw, trees @ [tree]

        let fm, trees = {1..nrounds} |> Seq.fold step (f0, [])
        getPred fm
        trees, fm

    let auc (pred : float32[]) (label : Covariate) =
        let prs = label.ToArray() |> Array.zip pred |> Array.sortByDescending fst
        let mutable sum_auc = 0.0f
        let mutable sum_pospair = 0.0f
        let mutable sum_npos = 0.0f
        let mutable sum_nneg = 0.0f
        let mutable buf_pos = 0.0f
        let mutable buf_neg = 0.0f
        for i in 0..pred.Length - 1 do
           let p, r = prs[i]
           let wt = 1.0f
           if i <> 0 && p <> fst(prs[i - 1]) then
               sum_pospair <- sum_pospair +  buf_neg * (sum_npos + buf_pos *0.5f)
               sum_npos <- sum_npos + buf_pos
               sum_nneg <- sum_nneg + buf_neg
               buf_neg <- 0.0f
               buf_pos <- 0.0f
           buf_pos <- buf_pos + r * wt
           buf_neg <- buf_neg + (1.0f - r) * wt
        sum_pospair <- sum_pospair + buf_neg * (sum_npos + buf_pos * 0.5f)
        sum_npos <- sum_npos + buf_pos
        sum_nneg <- sum_nneg + buf_neg
        sum_auc <- sum_auc + sum_pospair / (sum_npos * sum_nneg)
        sum_auc
