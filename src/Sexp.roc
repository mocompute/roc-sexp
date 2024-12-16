module [
    Sexp,
]

import Arena exposing [Arena]
import Token

bufCapacity = 1024

Sexp : [Nil, SNumber Dec, SSymbol Str, SString Str, SList (List Sexp)]

State : {
    source : List U8,
    arena : Arena Sexp,
    descentIndex : U64,
    prevDescentIndex : U64,
    currentIndex : U64,
}

stateInit = \source -> {
    source,
    arena: Arena.create bufCapacity Nil,
    descentIndex: 0,
    prevDescentIndex: 0,
    currentIndex: 0,
}
# stateResetBuf = \s -> { s &
#         arena: Arena.reset s.arena,
#         descentIndex: 0,
#         prevDescentIndex: 0,
#         currentIndex: 0,
#     }

stateAppendSexp = \s, sexp ->
    dbg ("appending", sexp, s.currentIndex)
    arena_1 =
        when Arena.append s.arena sexp is
            Ok res -> res
            _ -> crash "OOM"

    { s &
        arena: arena_1,
        currentIndex: s.currentIndex + 1,
    }

stateDescend = \s ->
    i = Arena.next s.arena
    { s &
        prevDescentIndex: s.descentIndex,
        descentIndex: i,
        currentIndex: i,
    }

stateAscend = \s ->
    i = s.prevDescentIndex
    { s &
        descentIndex: i,
    }

stateBufToSList : State -> (Sexp, State)
stateBufToSList = \s ->
    res =
        Arena.sublist s.arena { start: s.descentIndex, len: s.currentIndex - s.descentIndex }
        |> SList
    (res, s)

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
                    dbg "descending from parse"
                    when startSList (state |> stateDescend) nextTState is
                        Ok (sexp, _, _) -> Ok sexp
                        Err e -> Err e

                TCloseRound -> Err (InvalidToken CloseRound loc)
                _ -> crash "unreachable"

        Err e -> Err (TokenizerError e)

startSList : State, Token.State -> Result (Sexp, State, Token.State) _
startSList = \state, tokenState ->
    dbg (state.descentIndex, state.currentIndex)
    when Token.next tokenState is
        Ok ((tok, _loc), nextTState) ->
            when tok is
                TCloseRound ->
                    (res, state_1) = stateBufToSList state
                    dbg "returning"
                    dbg res
                    Ok (res, state_1 |> stateAscend, nextTState)

                TOpenRound ->
                    dbg "descending"
                    when startSList (state |> stateDescend) nextTState is
                        Ok (sexp, state_1, tokenState_1) ->
                            startSList (state_1 |> stateAppendSexp sexp) tokenState_1

                        other -> other

                TSymbol s ->
                    startSList (state |> stateAppendSexp (SSymbol s)) nextTState

                TString s ->
                    startSList (state |> stateAppendSexp (SString s)) nextTState

                TNumber s ->
                    startSList (state |> stateAppendSexp (SNumber s)) nextTState

        Err e -> Err (TokenizerError e)

# expect
#     source = "1"
#     res = parse (source |> Str.toUtf8) |> Result.withDefault Nil
#     res == SNumber 1

# expect
#     source = "(123)"
#     res = parse (source |> Str.toUtf8) |> Result.withDefault Nil
#     res == SList [SNumber 123]

expect
    source = "(123 (456) 789)"
    res = parse (source |> Str.toUtf8) |> Result.withDefault Nil
    dbg res
    res == SList [SNumber 123, SList [SNumber 456], SNumber 789]
