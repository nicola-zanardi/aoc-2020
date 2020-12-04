#r "nuget: Unquote"

open Swensen.Unquote
open System

let readTxt path =
    let fullPath = $"{__SOURCE_DIRECTORY__}\\{path}"

    System
        .IO
        .File
        .ReadAllText(fullPath)
        .Replace((Environment.NewLine + Environment.NewLine), "|")
        .Replace(Environment.NewLine, " ")
        .Split("|")

let inputTest = readTxt "04_test.txt"
let input = readTxt "04.txt"

let extractPassportMap (passportString: string) =
    let makeTuple (arr: string []) = (arr.[0], arr.[1])

    passportString.Trim().Split([| ' ' |])
    |> Array.map ((fun x -> x.Split(':')) >> makeTuple)
    |> Map.ofArray

let parsePassports input = input |> Array.map extractPassportMap

let validatePassport (passport: Map<string, string>) =
    let requiredFields =
        [ "byr"
          "iyr"
          "eyr"
          "hgt"
          "hcl"
          "ecl"
          "pid" ]

    let nrMissingRequiredFields =
        requiredFields
        |> List.map passport.ContainsKey
        |> List.filter ((=) false)
        |> List.length

    nrMissingRequiredFields = 0

let part1 input =
    parsePassports input
    |> Array.map validatePassport
    |> Array.filter ((=) true)
    |> Array.length

//////////////////////////////////////////////////////////////////////////
printfn "TESTING... "

test <@ part1 inputTest = 2 @>
// test <@ part2 inputTest part2Movements = 336UL @>

printfn "DONE"
//////////////////////////////////////////////////////////////////////////

printfn $"PART 1 => %i{part1 input}"
