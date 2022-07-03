#r "nuget: NumSharp"
#r "nuget: FeatherDotNet"
#time "on"

open System
open System.Collections
open NumSharp
open FeatherDotNet


type GenSeq<'a, 'b> = ValueOption<unit -> struct ('a ref * ('a ref -> struct (ValueOption<'b> * 'a ref)))>
    // | Nil
    // | Cons of (unit -> struct ('a ref * ('a ref -> struct (ValueOption<'b> * 'a ref))))

module GenSeq =

    let fromSeq (s : seq<'a>) =
        let state = ref (s.GetEnumerator())
        let next (e: Generic.IEnumerator<'a> ref) =
            if e.Value.MoveNext() then
                struct (ValueSome e.Value.Current, e)
            else
                struct (ValueNone, e)
        ValueSome(fun () -> struct(state, next))

    let create state next : GenSeq<'a, 'b> =
        ValueSome(fun () -> struct(state, next))

    let inline tryRead (source: GenSeq<'a, 'b>) =
        match source with
            | ValueNone -> struct (ValueNone, ValueNone)
            | ValueSome(f) ->
                let struct(state, next) = f()
                let struct (v, newState) = next state
                if v.IsNone then
                    match box state with
                        | :? IDisposable as d -> d.Dispose()
                        | _ -> ()
                    struct (v, ValueNone)
                else
                 struct (v, ValueSome(fun () -> struct(newState, next)))

    let inline map (mapping: 'a -> 'b) (source: GenSeq<'c, 'a>)  =
        match source with
            | ValueNone -> ValueNone
            | ValueSome(g) ->
                let g'() =
                    let struct(state, next) = g()
                    let next' s =
                        let struct (v, state') = next(s)
                        struct(ValueOption.map mapping v, state')
                    struct(state, next')
                ValueSome(g')

    let rec fold (folder: 'c -> 'b -> 'c) (state: 'c) (source: GenSeq<'a, 'b>) =
        let mutable acc = state
        let mutable hasData = true
        while hasData do
            match tryRead source with
                | struct(ValueSome v, _) ->
                    acc <- folder acc v
                | _ -> hasData <- false
        acc

        // match tryRead source with
        //     | struct(true, v, (Cons(_) as tail)) -> fold folder (folder state v) tail
        //     | _ -> state

    let reduce (reducer: 'b -> 'b -> 'b) (source: GenSeq<'a, 'b>) =
        match tryRead source with
            | struct(ValueSome v, (ValueSome(_) as tail)) -> fold reducer v tail
            | _ -> raise (new InvalidOperationException())

    let rec iter (action: 'b -> unit) (source: GenSeq<'a, 'b>) =
        match tryRead source with
           | struct(ValueSome v, (tail)) ->
               action v
               iter action tail
           | _ -> ()

    let rec filter (predicate: 'b -> bool) (source: GenSeq<'a, 'b>) =
        match source with
            | ValueNone -> ValueNone
            | ValueSome(g) ->
                let g'() =
                    let struct(state, next) = g()
                    let rec next' s =
                        let struct(v, state') = next(s)
                        match v with
                            | ValueSome(x) when predicate x -> struct(v, state')
                            | ValueSome(x) when predicate x |> not -> next' state'
                            | _ -> struct(ValueNone, state')
                    struct(state, next')
                ValueSome(g')

    // let inline take(count: int) (source: GenSeq<'a, 'b>) =
    //     match source with
    //         | Nil -> Nil
    //         | Cons(g) ->
    //             let g'() =
    //                 let struct(state, next) = g()
    //                 let next' (s', n') =
    //                     if n' >= 1 then
    //                         let struct(hasValue, v, newstate) = next(s')
    //                         struct(hasValue, v, (newstate, n' - 1))
    //                     else
    //                         struct(false, Unchecked.defaultof<'b>, (s', 0))
    //                 struct((state, count), next')
    //             Cons(g')

    let inline toList (source: GenSeq<'a, 'b>) : 'b list =
        let rec toList' acc (xs: GenSeq<'a, 'b>) =
            match tryRead xs with
            | struct(ValueSome v, (ValueSome(_) as tail)) -> toList' (v::acc) tail
            | _ -> acc
        toList' [] source |> List.rev

open GenSeq

//let xs = GenSeq<_, _>.create 0 (fun state -> (if state < 5 then (true, state, state + 1) else (false, 0, state + 1)) )

//let v = GenSeq.fold (+) 0 xs

//GenSeq.iter (fun x -> printfn "dupa: %d" x) (xs |> GenSeq.map (fun x -> 2 * x))

//GenSeq.iter (fun x -> printfn "dupa2: %d" x) (xs |> GenSeq.filter (fun x -> x % 2 = 0))

//GenSeq.iter (fun x -> printfn "dupa3: %d" x) (xs |> GenSeq.take 2)

//let v = xs |> GenSeq.toList

//let v2 = xs |> GenSeq.reduce (+)
let n = 100000000L
let testSeq = {1L..n}
#time "on"
let s1 = testSeq
                        //|> Seq.filter (fun x -> x % 2L = 0)
                        //|> Seq.map (fun x -> 2L * x)
                        //|> Seq.reduce (fun x y -> y)
                        |> Seq.iter (fun x -> ())
#time "off"

#time "on"
let state0 = ref 1L
let s2 = GenSeq.create state0 (fun state ->
    (if state.Value <= n then
        let v = state.Value
        state.Value <- v + 1L
        struct(ValueSome v, state)
    else
        struct(ValueNone, state)))
                              //|> GenSeq.filter (fun x -> x % 2L = 0)
                              //|> GenSeq.map (fun x -> 2L * x)
                              //|> GenSeq.reduce (fun x y -> y)
                              |> GenSeq.iter (fun x -> ())
#time "off"

#time "on"
let s3 = GenSeq.fromSeq testSeq
                              |> GenSeq.filter (fun x -> x % 2L = 0)
                              |> GenSeq.map (fun x -> 2L * x)
                              |> GenSeq.reduce (fun x y -> y)
                              //|> GenSeq.iter (fun x -> ())
#time "off"


