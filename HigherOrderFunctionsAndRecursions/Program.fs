open System

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

[<EntryPoint>]
let main argv =
    Console.WriteLine(fibonacciUp 15)
    Console.WriteLine(fibonacciDown 15)
    Console.WriteLine(sumOfDigitsDown 15)
    0