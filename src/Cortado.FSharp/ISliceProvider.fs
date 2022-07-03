namespace Cortado.FSharp

open System

type ISliceProvider<'a> =
    abstract member GetSlices: start:int * length:int * sliceLength:int -> seq<Memory<'a>>
