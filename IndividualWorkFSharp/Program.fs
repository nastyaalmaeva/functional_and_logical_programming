open System

// Функция проверки, содержит ли число все требуемые цифры (0, 1, A)
let containsRequiredDigits (s: string) =
    s.Contains("0") && s.Contains("1") && s.Contains("A")

// Функция генерации всех шестнадцатеричных чисел длиной до n цифр
let generateHexNumbers n =
    let hexDigits = [| '0'..'9' |] |> Array.append [| 'A'..'F' |]
    let rec generate length current =
        seq {
            if length = 0 then
                if current <> "" && current.[0] <> '0' then // Без ведущих нулей
                    yield current
            else
                for d in hexDigits do
                    yield! generate (length - 1) (current + string d)
        }
    seq { 1..n } |> Seq.collect (fun len -> generate len "")

// Основная функция
let countHexNumbersWithDigits n =
    generateHexNumbers n
    |> Seq.filter containsRequiredDigits
    |> Seq.length

// Получаем результат для 16 цифр и выводим в шестнадцатеричном формате
let result = countHexNumbersWithDigits 16
printfn "%X" result
