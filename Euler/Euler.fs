namespace Euler

module Euler =
    
    open System

    //generating Fibonacci sequence
    let fibonacci() = Seq.unfold (fun state ->
        if (fst state + snd state > Int32.MaxValue) then None
        else Some(fst state + snd state, (snd state, fst state + snd state))) (0,1)    

    //generating bigint Fibonacci sequence
    let fibonacciI() = Seq.unfold (fun state -> Some(fst state + snd state, (snd state, fst state + snd state))) (0I,1I)

    let fibonacciN (n : int) =
        let x1 = (1.0 + sqrt 5.0) /2.0
        let x2 = (1.0 - sqrt 5.0) / 2.0
        int ((x1**(float (n - 1)) - x2**(float (n - 1))) / sqrt 5.0)

    let fibonacciM() = Seq.initInfinite(fun x -> fibonacciN x)

    //factoral function
    let rec factorial n =
        match n with
        | 0 | 1 -> 1
        | _ -> n * factorial(n-1)

    //factorial bigint
    let rec factorialI (n : bigint) =
        match n with
        | _ when (n = 0I) -> 1I
        | _ when (n = 1I) -> 1I
        | _ -> n * factorialI(n-1I)
