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

[<EntryPoint>]
let main argv =
    Console.Write("Input a number: ")
    let number = Console.ReadLine() |> int
    
    Console.WriteLine($"Sum of mutually prime numbers with {number}: {traverseCoprimes number (+) 0}")
    Console.WriteLine($"Mult of mutually prime numbers with {number}: {traverseCoprimes number (*) 1}")
    Console.WriteLine($"Max of mutually prime numbers with {number}: {traverseCoprimes number (fun x y -> if x > y then x else y) 0}")
    Console.WriteLine()
    Console.WriteLine($"Euler function of {number}: {eulerFunction number}")
    Console.WriteLine()
    Console.WriteLine($"Mul of even mutually prime numbers with {number}: {traverseCoprimesCondition number (*) (fun x -> (x % 2) = 0) 1}")
    0
