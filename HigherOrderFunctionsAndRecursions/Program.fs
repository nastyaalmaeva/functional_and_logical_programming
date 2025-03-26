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

[<EntryPoint>]
let main argv =
    Console.WriteLine($"Sum of digits: {fibOrSumOfDigits true 15}")
    Console.WriteLine($"15th term in Fibonacci sequence: {fibOrSumOfDigits false 15}")
    Console.WriteLine()
    Console.WriteLine($"Sum of digits of number: {numberTraversal 124 (fun x y -> (x + y)) 0}")
    Console.WriteLine($"Mul of digits of number: {numberTraversal 124 (fun x y -> (x * y)) 1}")
    Console.WriteLine($"Minimum digit of number: {numberTraversal 421 (fun x y -> if x > y then y else x) 10}")
    Console.WriteLine($"Maximum digit of number: {numberTraversal 421 (fun x y -> if x < y then y else x) -1}")
    Console.WriteLine()
    Console.WriteLine($"Sum of even digits of number: {numberTraversalWithCondition 4561 (fun x y -> (x + y)) 0 (fun digit -> digit % 2 = 0)}")
    Console.WriteLine($"Mul of digits larger than 3: {numberTraversalWithCondition 4562 (fun x y -> (x * y)) 1 (fun digit -> digit > 3)}")
    Console.WriteLine($"Minimum odd digit of number: {numberTraversalWithCondition 4562 (fun x y -> if x > y then y else x) 10 (fun digit -> digit % 2 <> 0)}")
    0   