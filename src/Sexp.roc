module [
    Sexp,
]

import Token

bufCapacity = 1024

Sexp : [Nil, SNumber Dec, SSymbol Str, SString Str, SList (List Sexp)]

State : {
    source : List U8,
    buf : List Sexp,
    bufIndex : U64,
}

stateInit = \source -> { source, buf: List.repeat Nil bufCapacity, bufIndex: 0 }
stateResetBuf = \s -> { s & bufIndex: 0 }
stateAppendSexp = \s, sexp -> { s &
        buf: List.set s.buf s.bufIndex sexp,
        bufIndex: s.bufIndex + 1,
    }

stateBufToSList : State -> (Sexp, State)
stateBufToSList = \s ->
    res =
        s.buf
        |> List.takeFirst s.bufIndex
        |> SList
    (res, s |> stateResetBuf)

parse = \source ->
    state = stateInit source
    tokenState = Token.stateInit source

    when Token.next tokenState is
        Ok ((tok, loc), nextTState) ->
            when tok is
                TSymbol s -> Ok (SSymbol s)
                TString s -> Ok (SString s)
                TNumber s -> Ok (SNumber s)
                TOpenRound -> startSList state nextTState
                TCloseRound -> Err (InvalidToken CloseRound loc)
                _ -> crash "unreachable"

        Err e -> Err (TokenizerError e)

startSList : State, Token.State -> Result Sexp _
startSList = \state, tokenState ->
    when Token.next tokenState is
        Ok ((tok, _loc), nextTState) ->
            when tok is
                TCloseRound ->
                    (res, _nextState) = stateBufToSList state
                    Ok res

                TOpenRound -> startSList state nextTState
                TSymbol s -> startSList (stateAppendSexp state (SSymbol s)) nextTState
                TString s -> startSList (stateAppendSexp state (SString s)) nextTState
                TNumber s -> startSList (stateAppendSexp state (SNumber s)) nextTState

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
    source = "(123 (456))"
    res = parse (source |> Str.toUtf8) |> Result.withDefault Nil
    res == SList [SNumber 123, SList [SNumber 456]]
