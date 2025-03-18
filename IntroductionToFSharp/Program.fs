open System

let solve (a, b, c) =
    let D = b * b - 4.0 * a * c
    (((-b + sqrt(D)) / (2.0 * a)), ((-b - sqrt(D)) / (2.0 * a)))


[<EntryPoint>]
let main argv =
    Console.WriteLine(solve (1.0, 2.0, -3.0))
    0