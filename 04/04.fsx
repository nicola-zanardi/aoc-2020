#r "nuget: Unquote"

open Swensen.Unquote
open System

// Read inputs
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

// Setup Railway
type TwoTrack<'TEntity> =
    | Valid of 'TEntity
    | Invalid of string

let bind switchFunction =
    fun twoTrackInput ->
        match twoTrackInput with
        | Valid v -> switchFunction v
        | Invalid i -> Invalid i

let (>>=) twoTrackInput switchFunction = bind switchFunction twoTrackInput

// Parse passport strings
let extractPassportMap (passportString: string) =
    let makeTuple (arr: string []) = (arr.[0], arr.[1])

    passportString.Trim().Split([| ' ' |])
    |> Array.map ((fun x -> x.Split(':')) >> makeTuple)
    |> Map.ofArray

let parsePassports input = input |> Array.map extractPassportMap

// Validators
let validateContainsAllRequiredFields (passport: Map<string, string>) =
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

    if nrMissingRequiredFields = 0 then Valid passport else Invalid "Some fields are missing"

let validateBirthYear (passport: Map<string, string>) =
    let value = passport.["byr"] |> int
    if (value >= 1920 && value <= 2002) then Valid passport else Invalid "Invalid Birthday Year"

let validateIssueYear (passport: Map<string, string>) =
    let value = passport.["iyr"] |> int
    if (value >= 2010 && value <= 2020) then Valid passport else Invalid "Invalid Issue Year"

let validateExpirationYear (passport: Map<string, string>) =
    let value = passport.["eyr"] |> int
    if (value >= 2020 && value <= 2030) then Valid passport else Invalid "Invalid Expiration Year"

let validateHeight (passport: Map<string, string>) =
    let str = passport.["hgt"]
    let unit = str.[str.Length - 2..str.Length - 1]
    let value = str.[0..str.Length - 3] |> int

    if ((unit = "cm" && value >= 150 && value <= 193)
        || (unit = "in" && value >= 59 && value <= 76)) then
        Valid passport
    else
        Invalid "Invalid Height"


let validateHairColor (passport: Map<string, string>) =
    let str = passport.["hcl"]
    let prefix = str.[0]
    let color = str.[1..str.Length - 1]
    let colorSet = color |> Set.ofSeq

    let allowableChars =
        "0123456789abcdef".ToCharArray() |> Set.ofArray

    if (prefix = '#'
        && colorSet.IsSubsetOf allowableChars
        && color.Length = 6) then
        Valid passport
    else
        Invalid "Invalid Hair Color"

let validateEyeColor (passport: Map<string, string>) =
    let str = passport.["ecl"]

    let allowableColors =
        [ "amb"
          "blu"
          "brn"
          "gry"
          "grn"
          "hzl"
          "oth" ]
        |> Set.ofList

    if (allowableColors.Contains str) then Valid passport else Invalid "Invalid Eye Color"

let validatePassportID (passport: Map<string, string>) =
    let pid = passport.["pid"]
    let pidSet = pid |> Set.ofSeq

    let allowableChars =
        "0123456789".ToCharArray() |> Set.ofArray

    if (pidSet.IsSubsetOf allowableChars && pid.Length = 9)
    then Valid passport
    else Invalid "Invalid Passport PID"

let validatorPart2 passport =
    passport
    |> validateContainsAllRequiredFields
    >>= validateBirthYear
    >>= validateIssueYear
    >>= validateExpirationYear
    >>= validateHeight
    >>= validateHairColor
    >>= validateEyeColor
    >>= validatePassportID

// Validate
let solve input validator =
    parsePassports input
    |> Array.sumBy
        (validator
         >> (fun x ->
             match x with
             | Valid _ -> 1
             | Invalid _ -> 0))

//////////////////////////////////////////////////////////////////////////
printfn "TESTING... "

test <@ solve inputTest validateContainsAllRequiredFields = 2 @>
test <@ validateBirthYear ([ ("byr", "2002") ] |> Map.ofList) = Valid([ ("byr", "2002") ] |> Map.ofList) @>
test <@ validateBirthYear ([ ("byr", "2003") ] |> Map.ofList) = Invalid "Invalid Birthday Year" @>
test <@ validateHeight ([ ("hgt", "60in") ] |> Map.ofList) = Valid([ ("hgt", "60in") ] |> Map.ofList) @>
test <@ validateHeight ([ ("hgt", "190cm") ] |> Map.ofList) = Valid([ ("hgt", "190cm") ] |> Map.ofList) @>
test <@ validateHeight ([ ("hgt", "190in") ] |> Map.ofList) = Invalid "Invalid Height" @>
test <@ validateHeight ([ ("hgt", "190") ] |> Map.ofList) = Invalid "Invalid Height" @>
test <@ validateHairColor ([ ("hcl", "#123abc") ] |> Map.ofList) = Valid([ ("hcl", "#123abc") ] |> Map.ofList) @>
test <@ validateHairColor ([ ("hcl", "#123abz") ] |> Map.ofList) = Invalid "Invalid Hair Color" @>
test <@ validateHairColor ([ ("hcl", "123abc") ] |> Map.ofList) = Invalid "Invalid Hair Color" @>
test <@ validateEyeColor ([ ("ecl", "brn") ] |> Map.ofList) = Valid([ ("ecl", "brn") ] |> Map.ofList) @>
test <@ validateEyeColor ([ ("ecl", "wat") ] |> Map.ofList) = Invalid "Invalid Eye Color" @>
test <@ validatePassportID ([ ("pid", "000000001") ] |> Map.ofList) = Valid([ ("pid", "000000001") ] |> Map.ofList) @>
test <@ validatePassportID ([ ("pid", "0123456789") ] |> Map.ofList) = Invalid "Invalid Passport PID" @>

printfn "DONE"
//////////////////////////////////////////////////////////////////////////

printfn $"PART 1 => %i{solve input validateContainsAllRequiredFields}"
printfn $"PART 2 => %i{solve input validatorPart2}"
