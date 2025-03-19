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

let circleArea radius =
    Math.PI * Math.Pow(radius, 2)

let cylinderVolume radius height=
    (circleArea radius) * height


[<EntryPoint>]
let main argv =
    Console.Write("Enter radius: ")
    let radius = Console.ReadLine() |> float
    Console.Write("Enter height: ")
    let height = Console.ReadLine() |> float
    Console.WriteLine($"Cylinder volume: {cylinderVolume radius height}")
    0
