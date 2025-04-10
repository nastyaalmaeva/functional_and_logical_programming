﻿open System

let rec fibonacciUp number =
    match number with
        | 0 -> 0
        | 1 -> 1
        | n -> fibonacciUp(n - 1) + fibonacciUp(n - 2)

let rec fibonacciDown number =
    let rec fibonacciLoop pred now num =
        match num with
            | 0 -> now
            | _ -> fibonacciLoop now (now + pred) (num - 1) 
    fibonacciLoop 1 0 number

let rec sumOfDigitsDown number =
    let rec sumDigitsDownLoop number currentSum =
        if number = 0 then currentSum
        else
            let newNumber = number / 10
            let digit = number % 10
            let newSum = currentSum + digit
            sumDigitsDownLoop newNumber newSum
    sumDigitsDownLoop number 0

let fibOrSumOfDigits flag =
    match flag with
        | true -> sumOfDigitsDown
        | false -> fibonacciDown

let rec numberTraversal num (func :int->int->int) acc = 
     match num with
        | 0 -> acc
        | n -> numberTraversal (n / 10) func (func acc (n % 10))

let rec numberTraversalWithCondition num (func :int->int->int) acc (condition :int->bool) =
    match num with
        | 0 -> acc
        | n -> 
            let digit = n % 10
            let nextNumber = n / 10
            let flag = condition digit
            match flag with
                | false -> numberTraversalWithCondition nextNumber func acc condition
                | true -> numberTraversalWithCondition nextNumber func (func acc digit) condition

let favouriteProgrammingLanguage language =
    match language with
        | "F#" | "Prolog" -> "Sneaky one..."
        | "C++" -> "You are good programmer!"
        | "C#" -> "You are genius programmer"
        | "Python" -> "Are you ok?"
        | "R" -> "Oh..Big Data lover..."
        | "Java" -> "GOOD ONE"
        | _ -> "Don't know that one..."

let askAboutFavouriteLangLanguageSuperposition () = 
    Console.WriteLine("What is your favorite programming language?")
    (Console.ReadLine >> favouriteProgrammingLanguage >> Console.WriteLine)()

let askAboutFavouriteLangLanguageCurry () =
    Console.WriteLine("What is your favorite programming language?")
    let runLanguagePipeline getInput processLanguage printOutput = 
        let language = getInput()
        let response = processLanguage language
        printOutput response
    runLanguagePipeline Console.ReadLine favouriteProgrammingLanguage Console.WriteLine

let rec GCD a b =
    match b with
    | 0 -> a
    | _ -> GCD b (a % b)

let traverseCoprimes target op initial =
    let rec loop n op acc candidate =
        match candidate with
        | 0 -> acc
        | _ ->
            let newAcc = 
                if GCD n candidate = 1 then op acc candidate
                else acc
            loop n op newAcc (candidate - 1)
    loop target op initial target

let eulerFunction number =
    traverseCoprimes number (fun x y -> x + 1) 0

let traverseCoprimesCondition number (func :int->int->int) (condition :int->bool) initial =
    let rec traversal number (func :int->int->int) (condition :int->bool) acc candidate =
        match candidate with
        | 0 -> acc
        | _ ->
            let nextCandidate = candidate-1
            let isCoprime = if GCD number candidate = 1 then true else false
            let flag = condition candidate
            match flag, isCoprime with
                | true, true -> traversal number func condition (func acc candidate) nextCandidate
                | _, _ -> traversal number func condition acc nextCandidate
    traversal number func condition initial number

let isPrime n =
    let rec check i =
        i > n / 2 || (n % i <> 0 && check (i + 1))
    if n < 2 then false else check 2

let sumPrimeDivisors n =
    let rec loop divisor acc =
        if divisor > n / 2 then acc
        else
            let newAcc = if n % divisor = 0 && isPrime divisor then acc + divisor else acc
            loop (divisor + 1) newAcc
    if n < 2 then 0 else loop 2 0

let countOddDigitsGreaterThanThree n =
    let rec loop number acc =
        if number = 0 then acc
        else
            let digit = number % 10
            let newAcc = if digit > 3 && digit % 2 <> 0 then acc + 1 else acc
            loop (number / 10) newAcc
    loop (abs n) 0

let productSpecialDivisors n =
    let targetSum = sumOfDigitsDown n
    let rec loop divisor acc =
        if divisor = 0 then acc
        else
            let newAcc = 
                if n % divisor = 0 && sumOfDigitsDown divisor < targetSum then 
                    acc * divisor
                else
                    acc
            loop (divisor - 1) newAcc
    if n = 0 then 0 else loop (abs n) 1

[<EntryPoint>]
let main argv =
    Console.Write("Input a number: ")
    let number = Console.ReadLine() |> int

    Console.WriteLine($"Sum of prime divisions of {number}: {sumPrimeDivisors number}")
    Console.WriteLine($"Count of odd digits of {number} greater than 3: {countOddDigitsGreaterThanThree number}")
    Console.WriteLine($"Product of divisors of number whose sum of digits is less than sum of digits of original number: {productSpecialDivisors number}")
    0
