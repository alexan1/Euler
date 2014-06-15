namespace Euler

module Euler =
    
    open System

    //check if even
    let isEven x =
        x % 2 = 0

    //check if odd
    let isOdd x =
        not (isEven x)   

    //check if even bigint
    let isEvenI x =
        x % 2I = 0I

     //check if odd bigint
    let isOddI x =
        not (isEvenI x)   
    
    //generating Fibonacci sequence
    let fibonacci() = Seq.unfold (fun state ->
        if (fst state + snd state > Int32.MaxValue) then None
        else Some(fst state + snd state, (snd state, fst state + snd state))) (0,1)    

    //generating bigint Fibonacci sequence
    let fibonacciI() = Seq.unfold (fun state -> Some(fst state + snd state, (snd state, fst state + snd state))) (0I,1I)

    //another way to generate Fibonacci number
    //TODO: check how it works
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

    //get larget prime factor for int
    let largestPrimeFactor x = 
        let rec largestPrimeFactorRec x y =
            if x = y then y
            elif x % y = 0 then largestPrimeFactorRec (x / y) y
            else largestPrimeFactorRec x (y + 1)
        largestPrimeFactorRec x 2
    
    //get larget prime factor for bigint number
    let largestPrimeFactorI x = 
        let rec largestPrimeFactorRecI x y =
            if x = y then y
            elif x % y = 0I then largestPrimeFactorRecI (x / y) y
            else largestPrimeFactorRecI x (y + 1I)
        largestPrimeFactorRecI x 2I
    
    //check if papamert is Palindrome
    // generic x can be int or string
    let isPalindrome x =
        x.ToString() = new string(Array.rev (x.ToString().ToCharArray()))

    //generate triangle sequence
    let triangles() =
        Seq.unfold (fun (acc, state) -> Some (acc, (state + acc, state + 1))) (0, 1)
        |> Seq.skip 1

     //generate triangle bigint sequence
    let trianglesI() =
        Seq.unfold (fun (acc, state) -> Some (acc, (state + acc, state + 1I))) (0I, 1I)
        |> Seq.skip 1

    //next Collatz number
    let nextcollatz x =
        if x = 1 then 0
        else 
            if isEven x then x / 2
            else 3 * x + 1
    
    //generate Collatz sequence starting with x
    let rec collatz x =         
        Seq.unfold (fun state -> if (state = 0) then None else Some(state, nextcollatz state)) x

    //next Collatz number bigint
    let nextcollatzI x =
        if x = 1I then 0I
        else 
            if isEvenI x then x / 2I
            else 3I * x + 1I

    //generate bigint Collatz sequence starting with x
    let rec collatzI x =         
        Seq.unfold (fun state -> if (state = 0I) then None else Some(state, nextcollatzI state)) x