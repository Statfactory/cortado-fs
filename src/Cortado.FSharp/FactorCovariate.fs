namespace Cortado.FSharp

open System
open System.Collections.Generic
open Literals

type Factor(name : string, length : int, levels: string[], sliceProvider: ISliceProvider<int>, isOrdinal: bool) =
    do
        if length < 0 then failwith "Factor length must be > 0"

    member _.Name with get() = name
    member _.Length with get() = length
    member _.Levels with get() = levels
    member _.SliceProvider with get() = sliceProvider
    member _.IsOrdinal with get() = isOrdinal
    member _.AsCovariate(f: string -> float32) =

        let convert (buffer: Memory<float32>) (parsed: Memory<float32>) (slice: Memory<int>) =
            let n = slice.Length
            let sliceSpan = slice.Span
            let bufferSpan  = buffer.Span
            let parsedSpan = parsed.Span
            for i in 0..n - 1 do
                bufferSpan[i] <- parsedSpan[sliceSpan[i]]
            if buffer.Length > n then
                buffer.Slice(0, n)
            else
                buffer

        let parsed = levels |> Array.map f

        let sliceProvider =
            { new ISliceProvider<float32> with
                member _.GetSlices(start, length, sliceLen) =
                    let buffer = Array.zeroCreate<float32>(sliceLen)
                    sliceProvider.GetSlices(start, length, sliceLen) |> Seq.map (convert (buffer.AsMemory()) (parsed.AsMemory()))
            }
        Covariate(name, length, sliceProvider)

    member _.AsCached() =
        let cache = Array.zeroCreate<int>(length)

        let folder (offset: int) (slice: Memory<int>) =
            let n = slice.Length
            let cacheSpan = cache.AsSpan()
            let sliceSpan = slice.Span
            for i in 0..n - 1 do
                cacheSpan[offset + i] <- sliceSpan[i]
            offset + n

        sliceProvider.GetSlices(0, length, SliceLength) |> Seq.fold folder 0 |> ignore

        Factor(name, length, levels, VectorSlicer(cache), isOrdinal)


    override _.ToString() =
        let k = min HeadLength length
        let slices = sliceProvider.GetSlices(0, k, k)

        let f (acc: string) (slice: Memory<int>) =
            let span = slice.Span;
            let labels = Array.zeroCreate<string>(slice.Length)
            for i in 0..slice.Length - 1 do
                labels[i] <- levels[span[i]]
            acc + String.Join(' ', labels) + " "

        let datahead =  slices |> Seq.fold f ""
        let endPart = if k = length then ""  else "..."
        $"Factor {name} with {length} obs and {levels.Length - 1} levels: {datahead}{endPart}"

and Covariate(name : string, length : int, sliceProvider: ISliceProvider<float32>) =
    do
        if length < 0 then failwith "Covariate length must be > 0"

    new(name: string, vector: float32[]) =
        Covariate(name, vector.Length, VectorSlicer(vector))

    member this.Name with get() = name
    member this.Length with get() = length
    member this.SliceProvider with get() = sliceProvider
    member this.AsFactor() =

        let folder (acc:Dictionary<float32, float32>) (slice: Memory<float32>) =
            let span = slice.Span
            for i in 0..slice.Length - 1 do
                let v = span[i]
                if Single.IsNaN(v) |> not && acc.ContainsKey(v)|> not then
                    acc[v] <- v
            acc

        let rec searchSorted (cuts: Memory<float32>) (x: float32) (fromInd: int) (toInd : int) =
            if Single.IsNaN(x) then
                0
            else
                let cutsSpan = cuts.Span;
                let mid = fromInd + (toInd - fromInd) / 2
                let cut0 = cutsSpan[mid]
                let cut1 = cutsSpan[mid + 1]
                if (x > cut0 && x <= cut1) then
                    mid + 1
                else if (x <= cut0) then
                    searchSorted cuts x fromInd mid
                else
                    searchSorted cuts x mid toInd

        let convert (buffer: Memory<int>) (cuts: Memory<float32>) (slice: Memory<float32>) =
                    let n = slice.Length
                    let sliceSpan = slice.Span
                    let bufferSpan  = buffer.Span
                    let k = cuts.Length - 1
                    for i in 0..n - 1 do
                        bufferSpan[i] <- searchSorted cuts sliceSpan[i] 0 k
                    if buffer.Length > n then
                        buffer.Slice(0, n)
                    else
                        buffer

        let uniqueVals =
            let slices = sliceProvider.GetSlices(0, length, SliceLength)
            let acc0 = Dictionary<float32, float32>()
            let res = slices |> Seq.fold folder acc0
            let vals = res.Keys |> Seq.toArray
            vals |> Array.sortInPlace
            vals

        let n = uniqueVals.Length
        let cuts = Array.zeroCreate<float32>(n + 1)
        cuts[0] <- Single.NegativeInfinity
        for i in 0..n - 1 do
            if i = n - 1 then
                cuts[i + 1] <- Single.PositiveInfinity
            else
                cuts[i + 1] <- uniqueVals[i] + 0.5f * (uniqueVals[i + 1] - uniqueVals[i])

        let sliceProvider =
                    { new ISliceProvider<int> with
                        member _.GetSlices(start, length, sliceLen) =
                            let buffer = Array.zeroCreate<int>(sliceLen)
                            sliceProvider.GetSlices(start, length, sliceLen) |> Seq.map (convert (buffer.AsMemory()) (cuts.AsMemory()))
                    }

        let levels = Array.zeroCreate<string>cuts.Length
        levels[0] <- MissingLevel
        for i in 0..cuts.Length - 2 do
            let z = if i = 0 then "[" else "("
            levels[i + 1] <- $"{z}{cuts[i]},{cuts[i + 1]}]"

        Factor(name, length, levels, sliceProvider, true)

    member this.AsCached() =
        let cache = this.ToArray()

        Covariate(name, length, VectorSlicer(cache.AsMemory()))

    member this.ToArray() =
        let arr = Array.zeroCreate<float32>(length)

        let folder (offset: int) (slice: Memory<float32>) =
            let n = slice.Length
            let cacheSpan = arr.AsSpan()
            let sliceSpan = slice.Span
            for i in 0..n - 1 do
                cacheSpan[offset + i] <- sliceSpan[i]
            offset + n

        sliceProvider.GetSlices(0, length, SliceLength) |> Seq.fold folder 0 |> ignore
        arr

    override _.ToString() =
        let k = min HeadLength length
        let slices = sliceProvider.GetSlices(0, k, k)

        let f (acc: string) (slice: Memory<float32>) =
            let span = slice.Span;
            let labels = Array.zeroCreate<string>(slice.Length)
            for i in 0..slice.Length - 1 do
                let v = span[i]
                labels[i] <- if Single.IsNaN(v) then MissingLevel else v.ToString()
            acc + String.Join(' ', labels) + " "

        let datahead =  slices |> Seq.fold f ""
        let endPart = if k = length then ""  else "..."
        $"Covariate {name} with {length} obs: {datahead}{endPart}"
