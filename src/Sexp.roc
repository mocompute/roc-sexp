module [
    Sexp,
    number,
]

Sexp : [Nil, Number Dec, Symbol Str, String Str, List Sexp]

number = \x -> Number x

isNumber = \x ->
    when x is
        Number _ -> Bool.true
        _ -> Bool.false

expect isNumber (Number 5)
expect Bool.not (isNumber (Symbol "foo"))

#
# --
#
