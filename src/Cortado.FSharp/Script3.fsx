open System
open System.IO

[<Struct>]
type DataReader<'a> =
    | Nil
    | Cons of (unit -> struct('a  * (DataReader<'a>)))

module DataReader =

    let rec create (f : unit -> ValueOption<'a>) =
        let a = f()
        match a with
            | ValueSome(a) -> Cons(fun () -> struct(a, (create f)))
            | ValueNone -> Nil

    let rec createCsvReader (sr : StreamReader) =
        create (fun () ->
                    if sr.EndOfStream then ValueNone
                    else
                        sr.ReadLine() |> ValueSome
               )


    let rec zip (r1 : DataReader<'a>) (r2 : DataReader<'b>) =
        match r1, r2 with
            | Cons(f1), Cons(f2) ->
                let struct(a, taila) = f1()
                let struct(b, tailb) = f2()
                Cons(fun () -> struct(((a,b)), (zip (taila) (tailb))))
            | _ -> Nil



    let tryRead (r : DataReader<'a>) =
        match r with
            | Nil -> struct(ValueNone, Nil)
            | Cons(f) ->
                let struct(a, taila) = f()
                struct(ValueSome(a), taila)

    let rec map f (r : DataReader<'a>) =
        match r with
            | Nil -> Nil
            | Cons(x) ->
                let struct(a, taila) = x()
                Cons(fun () -> struct((f(a)),  map f (taila)))

    let zip3 (r1 : DataReader<'a>) (r2 : DataReader<'b>) (r3 : DataReader<'c>) =
         r3 |> zip r2 |> zip r1 |> map (fun (x1, (x2, x3)) -> x1, x2, x3)

    let rec iter f (r : DataReader<'a>) =
        match r with
            | Nil -> ()
            | Cons(x) ->
                let struct((a), taila) = x()
                f(a)
                iter f (taila)

    let iteri f (r : DataReader<'a>) =
        let rec iteri' f r i =
            match r with
                | Nil -> ()
                | Cons(x) ->
                    let struct((a), taila) = x()
                    f i a
                    iteri' f (taila) (i + 1)
        iteri' f r 0

    let rec take n (r : DataReader<'a>) =
        match r |> tryRead with
            | ValueSome(a), tail ->
                if n > 1 then
                    Cons(fun () -> a, take (n-1) tail)
                else
                    Cons(fun () -> a, Nil)
            | ValueNone, _ -> Nil

    let rec filter (p : 'a -> bool) (r : DataReader<'a>) =
        match r |> tryRead with
            | struct(ValueSome(a), tail) ->
                if p(a) then
                    Cons(fun () -> struct((a), filter p tail))
                else
                    filter p tail
            | struct(ValueNone, _) -> Nil

    let toList (r : DataReader<'a>) =
        let rec toList' acc (r : DataReader<'a>)  =
            match r |> tryRead with
                | ValueSome(a), tail ->
                    (tail |> toList' (a::acc))
                | ValueNone, _ -> acc
        r |> toList' [] |> List.rev

    let rec toSeq (r : DataReader<'a>) =
        seq
           {
            match r |> tryRead with
                | ValueSome(a), tail ->
                    yield a
                    yield! tail |> toSeq
                | _ -> ()
           }

    let rec fold f init (r : DataReader<'a>) =
        match r |> tryRead with
            | struct(ValueSome(v), tail) -> fold f (f init v) tail
            | _ -> init

    let reduce (reducer: 'a -> 'a -> 'a) (source: DataReader<'a>) =
        match tryRead source with
            | struct(ValueSome(v), tail) -> fold reducer v tail
            | _ -> raise (new InvalidOperationException())

    let rec concat (readers : DataReader<'a> list) =
        match readers with
            | [] -> Nil
            | h :: t ->
                match h |> tryRead with
                    | ValueSome(a), tail ->
                       Cons(fun () -> a, concat (tail::t))
                    | _ -> concat t


let n = 100000000L
#time "on"
let mutable state = 1L
let s2 = DataReader.create (fun () ->
    (if state <= n then
        let v = state
        state <- v + 1L
        ValueSome v
    else
       ValueNone))
                              //|> DataReader.filter (fun x -> x % 2L = 0)
                              //|> DataReader.map (fun x -> 2L * x)
                              //|> DataReader.reduce (fun x y -> y)
                              |> DataReader.iter (fun x -> ())
#time "off"

