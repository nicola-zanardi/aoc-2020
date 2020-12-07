#r "nuget: Unquote"

open Swensen.Unquote

// Read inputs
let readTxt path =
    let fullPath =
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, path)

    System.IO.File.ReadLines(fullPath) |> Seq.toList

let input = readTxt "05.txt"

//

let parsePartitioningString (str: string) =
    let rec inner (str: string) (seats: int list) =
        if seats.Length = 1 then
            seats.[0]
        else
            match str.[0] with
            | 'F'
            | 'L' -> inner str.[1..str.Length - 1] seats.[0..seats.Length / 2 - 1]
            | 'B'
            | 'R' -> inner str.[1..str.Length - 1] seats.[seats.Length / 2..seats.Length - 1]
            | _ -> failwith ($"Unexpected control char : '{string str.[0]}'")

    inner str [ 0 .. (int (2.0 ** (float str.Length)) - 1) ]

let findSeatPosition (str: string) =
    let rowString = str.[0..6]
    let columnString = str.[7..9]
    (parsePartitioningString rowString, parsePartitioningString columnString)


let calculateSeatID seatPosition =
    let r, c = seatPosition
    r * 8 + c

let part1 =
    input
    |> List.map (findSeatPosition >> calculateSeatID)
    |> List.max

//////////////////////////////////////////////////////////////////////////
printfn "TESTING... "

test <@ findSeatPosition "FBFBBFFRLR" = (44, 5) @>

printfn "DONE"
//////////////////////////////////////////////////////////////////////////
printfn $"PART 1 => %i{part1}"
