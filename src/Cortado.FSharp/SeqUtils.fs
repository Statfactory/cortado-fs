namespace Cortado.FSharp

open System

module SeqUtils =

    let zip4 (s1: seq<'a>) (s2: seq<'b>) (s3: seq<'c>) (s4: seq<'d>) =
        seq
            {
            let e1, e2, e3, e4 = s1.GetEnumerator(), s2.GetEnumerator(), s3.GetEnumerator(), s4.GetEnumerator()
            while e1.MoveNext() && e2.MoveNext() && e3.MoveNext() && e4.MoveNext() do
                yield e1.Current, e2.Current, e3.Current, e4.Current
            }

    let zipNMem ([<ParamArray>] xss: seq<Memory<'a>>[]) =
        if xss.Length = 0 then failwith "zipN requires at least 1 seq"
        elif xss.Length = 1 then
            xss[0]
        else
            seq
                {
                    let n = xss.Length
                    let e = xss |> Array.map (fun xs -> xs.GetEnumerator())
                    let mutable buffer = Array.empty<'a>
                    while e |> Array.map (fun x -> x.MoveNext()) |> Array.fold (&&) true do
                        let slices = e |> Array.map (fun x -> x.Current)
                        let sliceLen = slices[0].Length
                        if buffer.Length = 0 then
                            buffer <- Array.zeroCreate<'a> (n * sliceLen)

                        for i in 0..n-1 do
                            slices[i].CopyTo(buffer.AsMemory().Slice(i * sliceLen))

                        yield buffer.AsMemory().Slice(0, n * sliceLen)
                }

    let rec createSeq (state: 'a) (next: 'a -> Option<'b> * 'a) =
        seq
            {
                let v, newState = next(state)
                match v with
                    | Some(x) ->
                        yield x
                        yield! createSeq newState next
                    | None -> ()

            }

    let nextSlice((start, length, slicelength): int * int * int) =
        let slicelength' = min length slicelength
        let newStart = start + slicelength'
        let newLength = length - slicelength'
        if slicelength' <= 0 then
            None, (start, length, slicelength)
        else
            Some(start, slicelength'), (newStart, newLength, slicelength')
