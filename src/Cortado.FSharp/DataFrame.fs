namespace Cortado.FSharp

open System
open FeatherDotNet
open System.Reflection
open Literals

module DataFrameUtils =
    // FeatherDotNet hides metadata as internal, use reflection
    let getColMetaData(colSpec: obj) =
        let levelsInfo = colSpec.GetType().GetProperty("CategoryLevels")
        let levels = levelsInfo.GetValue(colSpec)
        let nameInfo = colSpec.GetType().GetProperty("Name")
        let name = nameInfo.GetValue(colSpec)
        let colTypeInfo = colSpec.GetType().GetProperty("Type")
        let colType = colTypeInfo.GetValue(colSpec)
        {|Name = name.ToString(); ColType = colType.ToString(); Levels = levels:?>string[] |}

    let getMetaData(df: FeatherDotNet.DataFrame) =
        let t = typeof<FeatherDotNet.DataFrame>
        let metadataInfo = t.GetField("Metadata", BindingFlags.NonPublic ||| BindingFlags.Instance)
        let metadata = metadataInfo.GetValue(df)
        let colsInfo = metadata.GetType().GetProperty("Columns")
        let cols = colsInfo.GetValue(metadata):?>System.Array
        let len = cols.Length
        [|0..len-1|] |> Array.map (fun i -> getColMetaData(cols.GetValue([|i|])))

    let getArray(col: Column, f: 'a -> 'b) =
        let mutable arr = Array.zeroCreate<'a>(0)
        col.GetRange<'a>(0L, int(col.Length), &arr)
        let res = Array.zeroCreate<'b>(arr.Length)
        let spanFrom = arr.AsSpan()
        let spanTo = res.AsSpan()
        for i in 0..arr.Length - 1 do
            spanTo[i] <- f(spanFrom[i])
        res

    let tryGetCovariate(col: Column) =
        let colType = col.Type
        if colType = typeof<int64> then
            let array = getArray(col, fun (x:int64) -> float32 x)
            Covariate(col.Name, int(col.Length), VectorSlicer(array)) |> Some
        else if colType = typeof<int32> then
            let array = getArray(col, fun (x:int32) -> float32 x)
            Covariate(col.Name, int(col.Length), VectorSlicer(array)) |> Some
        else if colType = typeof<float32> then
            let array = getArray(col, fun (x:float32) -> x)
            Covariate(col.Name, int(col.Length), VectorSlicer(array)) |> Some
        else if colType = typeof<float> then
            let array = getArray(col, fun (x:float) -> float32 x)
            Covariate(col.Name, int(col.Length), VectorSlicer(array)) |> Some
        else if colType = typeof<Nullable<int32>> then
            let array = getArray(col, fun (x:Nullable<int32>) -> if x.HasValue then float32(x.Value) else Single.NaN)
            Covariate(col.Name, int(col.Length), VectorSlicer(array)) |> Some
        else if colType = typeof<Nullable<int64>> then
            let array = getArray(col, fun (x:Nullable<int64>) -> if x.HasValue then float32(x.Value) else Single.NaN)
            Covariate(col.Name, int(col.Length), VectorSlicer(array)) |> Some
        else
            None

    let tryGetFactor(col: FeatherDotNet.Column, levels: string[]) =
        let colType = col.Type
        let newLevels = levels |> Array.insertAt 0 MissingLevel
        if colType = typeof<int8> then
            if levels.Length <= int Byte.MaxValue then
                let ndarray = getArray(col, fun (x:int8) -> int(x) + 1)
                Factor(col.Name, int(col.Length), newLevels, VectorSlicer(ndarray), false) |> Some
            else
                let ndarray = getArray(col, fun (x:int8) -> int(x) + 1)
                Factor(col.Name, int(col.Length), newLevels, VectorSlicer(ndarray), false) |> Some
        else if colType = typeof<int16> then
            let ndarray = getArray(col, fun (x:int16) -> int(x) + 1)
            Factor(col.Name, int(col.Length), newLevels, VectorSlicer(ndarray), false) |> Some
        else
            None

open DataFrameUtils

type DataFrame(covariates: seq<Covariate>, factors: seq<Factor>) =

    member _.Factors with get() = factors |> Seq.toArray

    member _.Covariates with get() = covariates |> Seq.toArray

    member _.GetFactor(name: string) = factors |> Seq.find (fun x -> x.Name = name)

    member _.GetCovariate(name: string) = covariates |> Seq.find (fun x -> x.Name = name)

    static member FromFeather(path: string) =

        let df = FeatherReader.ReadFromFile(path, BasisType.Zero)

        let metaData = getMetaData(df)

        let factors = metaData |> Array.filter (fun x -> x.Levels <> null)
                               |> Array.choose (fun x -> tryGetFactor(df.Columns[x.Name], x.Levels))

        let covariates = metaData |> Array.filter (fun x -> x.Levels = null)
                               |> Array.choose (fun x -> tryGetCovariate(df.Columns[x.Name]))
        DataFrame(covariates, factors)
