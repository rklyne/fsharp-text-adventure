
module game_parser

open FParsec

let test p str =
    match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "123.444"
test pfloat "123"

let s = pstring

let p1 = s "[" >>. pfloat .>> s "]"

test p1 "[]"
test p1 "[1]"


