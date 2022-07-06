namespace Cortado.FSharp
open SeqUtils
open System

type VectorSlicer<'a>(vector: Memory<'a>) =
    new(vector: 'a[]) = VectorSlicer(vector.AsMemory())

    interface ISliceProvider<'a> with

        member _.GetSlices(start, length, sliceLength) =
            if start < 0 then failwith "Slicing must start at obs >= 0"
            if start >= vector.Length then failwith "Slicing start must be < vector size"
            let length = min length (vector.Length - start)
            let slicelen = min length sliceLength
            let state = start, length, slicelen

            let f(x: int * int) =
                let start, length = x
                vector.Slice(start, length)

            createSeq state nextSlice |> Seq.map f


