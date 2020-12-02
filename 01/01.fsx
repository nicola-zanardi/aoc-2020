#r "nuget: Unquote"

open Swensen.Unquote

// let inputFile = "01_test.txt"
let inputFile = "01.txt"

let data =
    System.IO.File.ReadLines(inputFile)
    |> Seq.map int
    |> Seq.toList

let rec combination num list =
    match num, list with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | k, (x :: xs) ->
        (List.map ((@) [ x ]) (combination (k - 1) xs))
        @ combination k xs

let part1 =
    combination 2 data
    |> List.find (fun x -> List.sum x = 2020)
    |> List.reduce (*)

let part2 =
    combination 3 data
    |> List.find (fun x -> List.sum x = 2020)
    |> List.reduce (*)


// printfn "part 1: %i" part1
// printfn "part 2: %i" part2

printfn "TESTING... "

test <@ (1 + 2) / 3 = 0 @>
printfn "DONE"
