#r "nuget: Unquote"

open Swensen.Unquote

let readTxt path =
    let fullPath = $"{__SOURCE_DIRECTORY__}\\{path}"
    System.IO.File.ReadLines(fullPath) |> Seq.toList

let inputTest = readTxt "03_test.txt"
let input = readTxt "03.txt"


//    | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 |
//  0 | . | . | # | # | . | . | . | . | . | . | .  |
//  1 | # | . | . |(.)| # | . | . | . | # | . | .  |
//  2 | . | # | . | . | . | . |(#)| . | . | # | .  |
//  3 | . | . | # | . | # | . | . | . | # |(.)| #  |
//  4 | . |[#]| . | . | . | # | # | . | . | # | .  |
//  5 | . | . | # | . | # | # | . | . | . | . | .  |
//  6 | . | # | . | # | . | # | . | . | . | . | #  |
//  7 | . | # | . | . | . | . | . | . | . | . | #  |
//  8 | # | . | # | # | . | . | . | # | . | . | .  |
//  9 | # | . | . | . | # | # | . | . | . | . | #  |
// 10 | . | # | . | . | # | . | . | . | # | . | #  |

let countTrees (input: string list) dx dy =
    let repeatingWidth = input.[0].Length
    let maxHeight = input.Length

    let rec inner x y (trees: int64) =
        let x = x + dx
        let y = y + dy

        let x =
            if x >= repeatingWidth then x - repeatingWidth else x

        if y >= maxHeight then
            trees
        else
            // printfn $"x {x}\ty {y}" |> ignore
            match input.[y].[x] with
            | '.' -> inner x y (trees)
            | '#' -> inner x y (trees + 1L)
            | _ -> invalidArg "tree" "something weird happened"

    inner 0 0 0L

let part2Movements =
    [ (1, 1)
      (3, 1)
      (5, 1)
      (7, 1)
      (1, 2) ]

let part2 input movements =
    movements
    |> List.map (fun (dx, dy) -> countTrees input dx dy)
    |> List.reduce (*)


//////////////////////////////////////////////////////////////////////////
printfn "TESTING... "

test <@ countTrees inputTest 3 1 = 7L @>
test <@ part2 inputTest part2Movements = 336L @>

printfn "DONE"
//////////////////////////////////////////////////////////////////////////


printfn $"Part 1: %i{countTrees input 3 1}"
printfn $"Part 2: %i{part2 input part2Movements}"
