// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Euler.fs"
open Euler

let res = Euler.fibonacci() |> Seq.nth 9

let res1 = Euler.fibonacci() |> Seq.take 10 |> List.ofSeq 

let Bigres = Euler.fibonacciI() |> Seq.nth 10

let fac = Euler.factorial 10

let facI = Euler.factorialI 10I

let fibN = Euler.fibonacciN 4

let fibM = Euler.fibonacciM() |> Seq.take 10 |> List.ofSeq 


