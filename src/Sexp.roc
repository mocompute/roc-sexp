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

                TCloseRound -> Err (InvalidToken CloseRound loc)
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
