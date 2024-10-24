
let file (f: string) =
    let c =
        try open_in f
        with _ -> raise (EInput f)
    in
    let lb = Lexing.from_channel c in
    let p =
        try Parse.prog Lex.tkn lb
        with Parse.Error -> eraise (EParse (Lexing.lexeme lb))
    in
    Type.prog p;
    Causality.prog p;
    p
