namespace Euler

module Euler =
    
    open System

    let fibonacci() = Seq.unfold (fun state ->
        if (fst state + snd state > Int32.MaxValue) then None
        else Some(fst state + snd state, (snd state, fst state + snd state))) (0,1)

    let fibonacci1 = Seq.unfold (fun state ->
        if (fst state + snd state > Int32.MaxValue) then None
        else Some(fst state + snd state, (snd state, fst state + snd state))) (0,1)

    let fibonacciBig() = Seq.unfold (fun state ->Some(fst state + snd state, (snd state, fst state + snd state))) (0,1)