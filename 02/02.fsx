#r "nuget: Unquote"

open Swensen.Unquote

let readTxt path =
    System.IO.File.ReadLines(path) |> Seq.toList

let inputTest = readTxt "02_test.txt"
let input = readTxt "02.txt"

type Policy = { Letter: char; Nr1: int; Nr2: int }

let extractRules (line: string) =
    // 1-3 a: abcde
    // 1-3 b: cdefg
    // 2-9 c: ccccccccc
    let sections =
        line.Split ":"
        |> Seq.head
        |> (fun x -> x.Split " ")
        |> Seq.toList

    (sections.[0].Split("-") |> Seq.toList)
    @ [ sections.[1] ]

let parsePolicy (line: string) =
    let rules = extractRules line

    { Letter = rules.[2].ToCharArray().[0]
      Nr1 = int rules.[0]
      Nr2 = int rules.[1] }


let extractPassword (line: string) =
    line.Split ":" |> Seq.last |> (fun x -> x.Trim())



let validatePolicy policy password =
    let count x = Seq.filter ((=) x) >> Seq.length
    let nrOccurrencesLetter = count policy.Letter password

    (nrOccurrencesLetter <= int policy.Nr2)
    && (nrOccurrencesLetter >= int policy.Nr1)


let validatePolicyNew policy (password: string) =
    let lettersMatch a1 a2 = (a1 = a2)

    (lettersMatch password.[policy.Nr1 - 1] policy.Letter)
    <> (lettersMatch password.[policy.Nr2 - 1] policy.Letter)


let validateLine validator line =
    let pwd = extractPassword line
    let policy = parsePolicy line

    validator policy pwd

let countValidPassword policyValidator lines =
    let validator = validateLine policyValidator

    lines
    |> List.countBy validator
    |> List.filter (fun (b, _) -> b)
    |> List.head
    |> (fun (_, count) -> count)

//////////////////////////////////////////////////////////////////////////
printfn "TESTING... "

test <@ parsePolicy "1-3 a: abcde" = { Letter = 'a'; Nr1 = 1; Nr2 = 3 } @>

test <@ extractPassword "1-3 a: abcde" = "abcde" @>

test <@ extractPassword "1-3 b: cdefg" = "cdefg" @>
test <@ parsePolicy "1-3 b: cdefg" = { Letter = 'b'; Nr1 = 1; Nr2 = 3 } @>
test <@ validateLine validatePolicy "1-3 b: cdefg" = false @>
test <@ countValidPassword validatePolicy inputTest = 2 @>
test <@ countValidPassword validatePolicyNew inputTest = 1 @>
test <@ countValidPassword validatePolicy input = 591 @>
test <@ countValidPassword validatePolicyNew input = 335 @>

printfn "DONE"
//////////////////////////////////////////////////////////////////////////

printfn "PART 1 - %i" (countValidPassword validatePolicy input)
printfn "PART 2 - %i" (countValidPassword validatePolicyNew input)
