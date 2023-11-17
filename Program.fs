let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

let Highiest =
    salaries
    |> List.filter (fun salary -> salary > 100000)

printfn "Highiest Salaries: %A" Highiest
let calculateTax salary =
    match salary with
    | s when s <= 49020 -> int (float s * 0.15)
    | s when s <= 98040 -> int (float s * 0.205)
    | s when s <= 151978 -> int (float s * 0.26)
    | s when s <= 216511 -> int (float s * 0.29)
    | _ -> int (float salary * 0.33)

let taxes =
    salaries
    |> List.map calculateTax

printfn "Salaries: %A" salaries
printfn "Taxes: %A" taxes

let adjustedSalaries =
    salaries
    |> List.filter (fun salary -> salary < 49020)
    |> List.map (fun salary -> salary + 20000)

printfn "Adjusted Salaries: %A" adjustedSalaries

let sumMidSalaries =
    salaries
    |> List.filter (fun salary -> salary >= 50000 && salary <= 100000)
    |> List.fold (+) 0

printfn " The Sum of Salaries from $50,000 to $100,000: %d" sumMidSalaries

let sum d =
    let rec x cal a =
        if a > d then
            cal
        else
            x (cal + a) (a + 3)
    
    x 0 3

let result = sum 27
printfn "Result: %d" result