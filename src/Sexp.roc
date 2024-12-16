module [
    Sexp,
]

import Token

initialSListCapacity = 16

Sexp : [Nil, SNumber Dec, SSymbol Str, SString Str, SList (List Sexp)]

State : {
    source : List U8,
}

stateInit = \source -> {
    source,
}

## Return a string representation of the Sexp.
toString : Sexp -> Str
toString = \sexp ->
    Str.joinWith
        (toString_0 sexp (List.withCapacity initialSListCapacity))
        " "

toString_0 : Sexp, List Str -> List Str
toString_0 = \sexp, builder ->
    when sexp is
        Nil -> List.append builder "()"
        SNumber x -> List.append builder (Num.toStr x)
        SSymbol x -> List.append builder x
        SString x -> List.append builder "\"$(x)\""
        SList slist ->
            subBuilder = List.withCapacity initialSListCapacity
            f = \state, elem -> toString_0 elem state
            List.append builder "("
            |> List.concat (List.walk slist subBuilder f)
            |> List.append ")"


## Parse a List U8 (e.g. from Str.toUtf8) into a Sexp. On InvalidToken
## error, the token is returned along with its location.
parse : List U8 -> Result Sexp [InvalidToken Token.Token U64, TokenizerError Token.TokenError]
parse = \source ->
    state = stateInit source
    tokenState = Token.stateInit source

    when Token.next tokenState is
        Ok ((tok, loc), nextTState) ->
            when tok is
                TSymbol s -> Ok (SSymbol s)
                TString s -> Ok (SString s)
                TNumber s -> Ok (SNumber s)
                TOpenRound ->
                    when
                        startSList
                            state
                            nextTState
                            (List.withCapacity initialSListCapacity)
                    is
                        Ok (sexp, _, _) -> Ok sexp
                        Err e -> Err e

                TCloseRound -> Err (InvalidToken TCloseRound loc)
                _ -> crash "unreachable"

        Err e -> Err (TokenizerError e)

startSList : State, Token.State, List Sexp -> Result (Sexp, State, Token.State) _
startSList = \state, tokenState, slist ->
    when Token.next tokenState is
        Ok ((tok, _loc), nextTState) ->
            when tok is
                TCloseRound ->
                    Ok (SList slist, state, nextTState)

                TOpenRound ->
                    when
                        startSList
                            state
                            nextTState
                            (List.withCapacity initialSListCapacity)
                    is
                        Ok (sexp, state_1, tokenState_1) ->
                            startSList
                                state_1
                                tokenState_1
                                (List.append slist sexp)

                        other -> other

                TSymbol s ->
                    startSList
                        state
                        nextTState
                        (List.append slist (SSymbol s))

                TString s ->
                    startSList
                        state
                        nextTState
                        (List.append slist (SString s))

                TNumber s ->
                    startSList
                        state
                        nextTState
                        (List.append slist (SNumber s))

        Err e -> Err (TokenizerError e)

expect
    source = "1"
    res = parse (source |> Str.toUtf8) |> Result.withDefault Nil
    res == SNumber 1

expect
    source = "(123)"
    res = parse (source |> Str.toUtf8) |> Result.withDefault Nil
    res == SList [SNumber 123]

expect
    source = "(123 (456 654) 789)"
    res = parse (source |> Str.toUtf8) |> Result.withDefault Nil
    res == SList [SNumber 123, SList [SNumber 456, SNumber 654], SNumber 789]

expect
    in = SList [SNumber 123, SList [SNumber 456, SNumber 654], SNumber 789]
    expected = "( 123.0 ( 456.0 654.0 ) 789.0 )"
    out = toString in
    out == expected
