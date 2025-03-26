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

[<EntryPoint>]
let main argv =
    askAboutFavouriteLangLanguageSuperposition ()
    Console.WriteLine()
    askAboutFavouriteLangLanguageCurry ()
    0   