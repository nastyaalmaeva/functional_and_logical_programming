open System

type SolveResult =
    None
    | Linear of float
    | Quadratic of float*float

let solve a b c =
    let D = b * b - 4.0 * a * c
    if a = 0.0 then
        if b = 0.0 then None
        else Linear(-c / b)
    else
        if D < 0.0 then None
        else Quadratic((((-b + sqrt(D)) / (2.0 * a)), ((-b - sqrt(D)) / (2.0 * a))))


[<EntryPoint>]
let main argv =
    let result = solve 1.0 -2.0 1.0
    match result with
        None -> Console.WriteLine("There are no solutions.")
        | Linear(x) -> Console.WriteLine($"Linear equation: {x}.")
        | Quadratic(x1, x2) -> Console.WriteLine($"Quadratic equation: {x1}, {x2}.")
    0
