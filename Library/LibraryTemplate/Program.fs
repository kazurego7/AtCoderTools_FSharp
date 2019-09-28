module AtCoder


open Microsoft.FSharp.Collections
open System
open System.Collections
open System.Collections.Generic

#nowarn "0025" // パターンマッチが不完全である警告の無効

module Seq =
    let interval startInclusive endExclusive = seq { startInclusive..(endExclusive - 1) }
    let inline mapAdjacent (op : 'a -> 'a -> 'b) (source : seq<'a>) : seq<'b> =
        match source with
        | source when Seq.isEmpty source -> Seq.empty
        | _ -> Seq.map2 op source (Seq.tail source)

module InputOutputs =
    let read() : string = Console.ReadLine()
    let reads() : string [] = read().Split()

    let readMatrix() : string [,] =
        let mutable lines = []
        let line = reads() |> Array.toSeq
        while not (Seq.isEmpty line) do
            lines <- line :: lines
        lines
        |> Seq.rev
        |> array2D

    let readInt32() : int32 = read() |> int32
    let readInt64() : int64 = read() |> int64
    let inline int32s (source : 'a []) : int32 [] = Array.map int32 source
    let inline int64s (source : 'a []) : int64 [] = Array.map int64 source
    let readInt32s() : int32 [] = reads() |> int32s
    let readInt64s() : int64 [] = reads() |> int64s
    let inline print (item : 'a) : unit = printfn "%A" item

    let inline printRow (line : seq<'a>) : unit =
        let strs = line |> Seq.map string
        if Seq.isEmpty strs then
            printf "%s" (Seq.head strs)
            for s in Seq.skip 1 strs do
                printf " %s" s
        printf "\n"

    let inline printColumn (line : seq<'a>) : unit =
        for item in line do
            print item

    let inline printGridGraph (lines : 'a [,]) : unit =
        for i in (Seq.interval 0 lines.Length) do
            lines.[i, *]
            |> Seq.map string
            |> String.concat " "
            |> print

module NumericFunctions =
    type Mods =
        { divisor : int32 }

        member this.Mod(a : int64) =
            let b = a % int64 this.divisor |> int32
            if b < 0 then b + this.divisor
            else b

        member this.Mod(a : int32) = this.Mod(int64 a)
        member this.Add (a : int32) (b : int32) : int32 = (this.Mod a + this.Mod b) % this.divisor

        member this.Sub (a : int32) (b : int32) : int32 =
            let sub = (this.Mod a - this.Mod b) % this.divisor
            if sub < 0 then sub + this.divisor
            else sub

        member this.Mul (a : int32) (b : int32) : int32 = (int64 (this.Mod a) * int64 (this.Mod b)) % int64 this.divisor |> int32

        /// 二分累積 O(Log N)
        member this.Pow (b : int32) (n : int32) : int32 =
            let digit = int32 (Math.Log(float n, 2.0))

            let seqs =
                seq { 0..digit }
                |> Seq.scan (fun acm _ -> this.Mul acm acm) b
                |> Seq.toArray
            seq { 0..digit }
            |> Seq.fold (fun acm i ->
                   if ((n >>> i) &&& 1) = 1 then this.Mul acm seqs.[i]
                   else acm) 1

        /// フェルマーの小定理より
        member this.Inv(a : int32) : int32 = this.Pow a (this.divisor - 2)

        member this.Div (a : int32) (b : int32) : int32 = this.Mul a (this.Inv b)

        member this.Perm (n : int32) (k : int32) : int32 =
            match (n, k) with
            | (n, _) when n < 0 -> invalidArg "n" "n >= 0"
            | (_, k) when k < 0 -> invalidArg "k" "k >= 0"
            | (n, k) when k > n -> 0
            | _ -> seq { n - k + 1..n } |> Seq.fold this.Mul 1

        member this.FactTable(nMax : int32) : int32 [] =
            seq { 1..nMax }
            |> Seq.scan this.Mul 1
            |> Seq.toArray

        /// パスカルの三角形 O(N^2)
        member this.CombTable(nMax : int32) : int32 [,] =
            let table = Array2D.zeroCreate (nMax + 1) (nMax + 1)
            for n in 0..nMax do
                for k in 0..nMax do
                    match (n, k) with
                    | (n, k) when n < k -> table.[n, k] <- 0
                    | (_, k) when k = 0 -> table.[n, k] <- 1
                    | _ -> table.[n, k] <- int64 table.[n - 1, k - 1] + int64 table.[n - 1, k] % int64 this.divisor |> int32
            table

    let isEven (a : int64) : bool = a % 2L = 0L
    let isOdd (a : int64) : bool = not (isEven a)

    /// ユークリッドの互除法 O(Log N)
    let rec gcd (m : int64) (n : int64) : int64 =
        match (m, n) with
        | (m, _) when m <= 0L -> invalidArg "m" "m <= 0"
        | (_, n) when n <= 0L -> invalidArg "n" "n <= 0"
        | (m, n) when m < n -> gcd n m
        | (m, n) when m % n = 0L -> n
        | _ -> gcd n (m % n)

    /// gcdを使っているため O(Log N)
    let lcm (m : int64) (n : int64) : int64 = (bigint m) * (bigint n) / bigint (gcd m n) |> Checked.int64

    /// O(√N)
    let divisors (m : int64) : seq<int64> =
        match m with
        | m when m <= 0L -> invalidArg "m" "m <= 0"
        | _ ->
            let sqrtM = int (sqrt (float m))

            let overRootM =
                Seq.interval 1 (sqrtM + 1)
                |> Seq.map int64
                |> Seq.filter (fun d -> m % d = 0L)
                |> Seq.rev
            overRootM
            |> if int64 sqrtM * int64 sqrtM = m then Seq.tail
               else id
            |> Seq.map (fun x -> m / x)
            |> Seq.append overRootM

    /// O(√N)
    let rec commonDivisor (m : int64) (n : int64) : seq<int64> =
        match (m, n) with
        | (_, n) when n <= 0L -> invalidArg "n" "n <= 0"
        | (m, n) when m < n -> commonDivisor n m
        | _ -> divisors m |> Seq.filter (fun md -> n % md = 0L)

    /// エラトステネスの篩 O(N loglog N)
    let primes (n : int32) : seq<int32> =
        match n with
        | n when n <= 1 -> invalidArg "n" "n <= 1"
        | _ ->
            let mutable ps = Seq.interval 2 (n + 1)
            while not (Seq.isEmpty ps) && Seq.head ps <= int32 (sqrt (float n)) do
                let m = Seq.head ps
                ps <- seq { yield m } |> Seq.append (Seq.filter (fun p -> p % m <> 0) ps)
            ps

    /// 素因数分解 O(N loglog N)
    /// インデックスの数 -> 指数　の対応を持つ配列を返す
    let primeFactrization (n : int32) : int32 [] =
        match n with
        | n when n <= 1 -> invalidArg "n" "n <= 1"
        | _ ->
            let ps = primes n |> Seq.toArray

            let rec helper (quotient : int32) (i : int32) (primeCounts : int32 []) : int32 [] =
                match quotient with
                | quotient when quotient = 1 -> primeCounts
                | quotient when quotient % ps.[i] <> 0 -> helper quotient (i + 1) primeCounts
                | _ ->
                    primeCounts.[ps.[i]] <- primeCounts.[ps.[i]] + 1
                    helper (quotient / ps.[i]) (i + 1) primeCounts
            helper n 0 (Array.zeroCreate (n + 1))

module Algorithm =
    let rec binarySearch (source : 'a []) (predicate : 'a -> bool) (ng : int32) (ok : int32) : int32 =
        match (ok, ng) with
        | (ok, ng) when abs (ok - ng) = 1 -> ok
        | _ ->
            let mid = (ok + ng) / 2
            if predicate source.[mid] then binarySearch source predicate ng mid
            else binarySearch source predicate mid ok

    let leftBinarySearch (source : 'a []) (predicate : 'a -> bool) : int32 option =
        match source.Length with
        | len when len = Int32.MaxValue -> invalidArg "source" "It is equal Int32."
        | _ ->
            binarySearch source predicate source.Length -1
            |> fun ix ->
                if ix < 0 then None
                else Some ix

    let rightBinarySearch (source : 'a []) (predicate : 'a -> bool) =
        match source.Length with
        | len when len = Int32.MaxValue -> invalidArg "source" "It is equal Int32."
        | _ ->
            binarySearch source predicate -1 source.Length
            |> fun ix ->
                if ix >= source.Length then None
                else Some ix

    let runLengthEncoding (source : string) : seq<string * int32> =
        match source.Length with
        | n when n = 0 -> Seq.empty
        | n ->
            let cutIxs =
                Seq.interval 1 n
                    |> Seq.filter (fun i -> source.[i] <> source.[i - 1])
            Seq.append (Seq.append (seq { yield 0 }) cutIxs) (seq { yield n })
                |> Seq.mapAdjacent (fun i0 i1 -> (string source.[i0], i1 - i0))

    let rec ternarySearchDownward (left : float) (right : float) (convexFunction : float -> float) (allowableError : float) =
        match left, right, convexFunction, allowableError with
        | l, r, f, e when r - l < e -> l
        | l, r, f, e ->
            let ml = l + (r - l) / 3.0
            let mr = l + (r - l) / 3.0 * 2.0
            if f ml < f mr then ternarySearchDownward l mr f e
            else if f ml > f mr then ternarySearchDownward ml r f e
            else ternarySearchDownward ml mr f e

    let rec ternarySearchUpward (left : float) (right : float) (convexFunction : float -> float) (allowableError : float) =
        match left, right, convexFunction, allowableError with
        | l, r, f, e when r - l < e -> l
        | l, r, f, e ->
            let ml = l + (r - l) / 3.0
            let mr = l + (r - l) / 3.0 * 2.0
            if f ml < f mr then ternarySearchUpward ml r f e
            else if f ml > f mr then ternarySearchUpward l mr f e
            else ternarySearchUpward ml mr f e

module DataStructure =
    module UnionFind =
        type private Id = Int32
        type UnionFind(n) =
            let mutable parent : Id [] = Array.init n id
            let mutable size : int32 [] = Array.create n 1

            let rec root u =
                if parent.[u] = u then u
                else
                    let rootParent = root parent.[u]
                    parent.[u] <- rootParent
                    rootParent

            member this.Unite (u : Id) (v : Id) : unit =
                if root u = root v then ()
                else
                    parent.[root u] <- root v
                    size.[root v] <- size.[root u] + size.[root v]

            member this.Size(u : Id) : int32 =
                size.[root u]

            member this.Find (u : Id) (v : Id) : bool =
                root u = root v

    let reverseCompare (x : 'a) (y : 'a) : int32 = compare x y * -1

    module PriorityQueue =
        type PriorityQueue<'a when 'a : comparison and 'a : equality>(values : seq<'a>, comparison : 'a -> 'a -> int32) =
            let dict =
                let sorted = SortedDictionary<'a, Queue<'a>>(ComparisonIdentity.FromFunction comparison)
                Seq.groupBy id values
                    |> Seq.iter (fun (key, value) -> sorted.Add(key, (new Queue<'a>(value))))
                sorted

            let mutable size = dict.Count
            new(values : seq<'a>) = PriorityQueue(values, compare)

            member this.Enqueue(item : 'a) : unit =
                if dict.ContainsKey item then dict.[item].Enqueue(item)
                else
                    let added = new Queue<'a>()
                    added.Enqueue(item)
                    dict.Add(item, added)
                size <- size + 1

            member this.Peek =
                if Seq.isEmpty dict then invalidOp "queue is Empty"

                (Seq.head dict).Value |> Seq.head
            member this.Size = size
            member this.Dequeue() : 'a =
                if Seq.isEmpty dict then invalidOp "queue is Empty"

                let first = Seq.head dict
                if first.Value.Count <= 1 then dict.Remove(first.Key) |> ignore

                size <- size - 1
                first.Value.Dequeue()
            interface IEnumerable<IEnumerable<'a>> with
                member this.GetEnumerator() =
                    (seq { for kv in dict -> seq kv.Value }).GetEnumerator()
            interface IEnumerable with
                member this.GetEnumerator() =
                    (this :> IEnumerable<IEnumerable<'a>>).GetEnumerator() :> IEnumerator


open InputOutputs
open NumericFunctions

[<EntryPoint>]
let main _ =

    0 // return an integer exit code
