namespace Cortado.FSharp

open System
open Literals
open XGSplit

module Logistic =
    let inline logit_g (yhat:float32) (y: float32) =
        yhat - y

    let inline logit_h(yhat: float32) =
        let eps = Single.Epsilon
        max (yhat * (1.0f - yhat)) eps

    let inline logitraw(p: float32) =
        -Math.Log(1.0 / float(p) - 1.0) |> float32

    let inline sigmoid(x: float32) =
        1.0 / (1.0 + Math.Exp(-float(x))) |> float32

    let inline get_gh (yhatraw: float32[]) (label: Covariate) (g: float32[]) (h: float32[]) (sliceLen: int) =
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
                gSpan[offset + i] <- logit_g yhat y
                hSpan[offset + i] <- logit_h yhat
            offset + slice.Length
        ) 0 |> ignore
        ()

    let inline get_pred (fm: float32[]) =
        let fm = fm.AsMemory().Span
        for i in 0..fm.Length - 1 do
            fm[i] <- sigmoid(fm[i])

    let xgblogit (label: Covariate) (factors: seq<Factor>) (mu: float32)
                   (eta: float32) (lambda: float32) (gamma: float32)
                   (maxDepth: int) (nrounds: int) (minh: float32) (slicelen: int) =

        let n = label.Length
        let factors = factors |> Seq.toArray
        let f0 = Array.create n (logitraw(mu))
        let g = Array.zeroCreate<float32> n
        let h = Array.zeroCreate<float32> n

        let step (acc) (m) =
            let fm, trees = acc

            get_gh fm label g h slicelen
            let g_cov = Covariate("g", g)
            let h_cov = Covariate("h", h)
            let tree, predraw = growTree factors g_cov h_cov fm eta maxDepth lambda gamma minh slicelen
            predraw, trees @ [tree]

        let fm, trees = {1..nrounds} |> Seq.fold step (f0, [])
        get_pred fm
        trees, fm
