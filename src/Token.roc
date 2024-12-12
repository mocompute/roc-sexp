module [Token, init, next]

bufCapacity = 256

Token : [OpenRound, CloseRound, Symbol Str, String Str, Number Dec]
TokenError : [InvalidToken U64, Eof U64, BadUtf8 U64]

TokenOrError : [Ok Token, Err TokenError]

State : {
    source : List U8,
    index : U64,
    start : U64,
    buf : List U8,
    bufIndex : U64,
}

init : List U8 -> State
init = \source -> { source, index: 0, start: 0, buf: List.repeat 0 bufCapacity, bufIndex: 0 }

listClear = \list -> listClear1 list (List.len list) 0
listClear1 = \list, len, idx ->
    if idx >= len then
        list
    else
        listClear1 (List.set list idx 0) len (idx + 1)

resetBuf = \state -> { state & buf: listClear state.buf, bufIndex: 0 }
incrIndex = \state -> { state & index: state.index + 1 }
appendChar = \state, c ->
    { state &
        buf: List.set state.buf state.bufIndex c,
        bufIndex: state.bufIndex + 1,
    }

next : State -> Result (Token, State) TokenError
next = \state ->
    { source, index } = state
    when List.get source index is
        Ok '(' -> Ok (OpenRound, { state & index: index + 1 })
        Ok ')' -> Ok (CloseRound, { state & index: index + 1 })
        Ok '"' -> stringStart { state & start: index + 1, index: index + 1 }
        Ok ' ' | Ok '\n' | Ok '\r' | Ok '\t' -> next { state & index: index + 1 }
        Ok _ -> Err (Eof 0)
        Err OutOfBounds -> Err (Eof index)

stringStart : State -> Result (Token, State) TokenError
stringStart = \state ->
    { source, index } = state
    when List.get source index is
        Ok '"' ->
            # end of string
            res =
                state.buf
                |> List.takeFirst state.bufIndex
                |> Str.fromUtf8
                |> Result.mapErr \_ -> Err (BadUtf8 index)
            Result.try res \r -> Ok (String r, state |> resetBuf |> incrIndex)

        Ok '\\' -> stringBackslash { state & index: (index + 1) }
        Ok c -> stringStart (state |> appendChar c |> incrIndex)
        Err OutOfBounds -> Err (Eof index)

stringBackslash = \state ->
    { source, index } = state
    when List.get source index is
        Ok '\\' -> stringStart (state |> appendChar '\\' |> incrIndex)
        Ok c -> stringStart (state |> appendChar c |> incrIndex)
        Err OutOfBounds -> Err (Eof index)

#
# tests
#

WalkState : { tokState : State, break : [None, Mismatch, Error] }

check_ : WalkState, TokenOrError -> [Break WalkState, Continue WalkState]
check_ = \st, expected ->
    { tokState } = st

    when next tokState is
        Ok (tok, nextTokState) ->
            when expected is
                Ok good ->
                    if tok == good then
                        Continue { st & tokState: nextTokState }
                    else
                        dbg "\nexpected: $(Inspect.toStr good)\nactual:   $(Inspect.toStr tok)"
                        Break { st & break: Mismatch }

                Err err ->
                    dbg "\nexpected: $(Inspect.toStr err)\nactual:   $(Inspect.toStr tok)"
                    Break { st & break: Mismatch }

        Err (Eof index) ->
            when expected is
                Ok good ->
                    dbg "\nexpected: $(Inspect.toStr good)\nactual:   $(Inspect.toStr (Eof index))"
                    Break { st & break: Mismatch }

                Err (Eof expectedIndex) ->
                    if index == expectedIndex then
                        Break st
                    else
                        dbg "\nexpected: $(Inspect.toStr (Eof expectedIndex))\nactual:   $(Inspect.toStr (Eof index))"
                        Break { st & break: Mismatch }

                Err _ ->
                    Break { st & break: Error }

        Err _ ->
            Break { st & break: Error }

#

expect
    source = "()"
    good = [Ok OpenRound, Ok CloseRound, Err (Eof 2)]

    state = init (Str.toUtf8 source)

    walkState = { tokState: state, break: None }
    res = List.walkUntil
        good
        walkState
        check_

    when res.break is
        None -> Bool.true
        _ -> Bool.false

expect
    source = "(\"string 1\" \"string 2\")"
    good = [Ok OpenRound, Ok (String "string 1"), Ok (String "string 2"), Ok CloseRound, Err (Eof 23)]

    state = init (Str.toUtf8 source)

    walkState = { tokState: state, break: None }
    res = List.walkUntil
        good
        walkState
        check_

    when res.break is
        None -> Bool.true
        _ -> Bool.false

expect
    source = "\"hello back\\\\slash\""
    good = [Ok (String "hello back\\slash"), Err (Eof 19)]

    state = init (Str.toUtf8 source)
    walkState = { tokState: state, break: None }
    res = List.walkUntil
        good
        walkState
        check_

    when res.break is
        None -> Bool.true
        _ -> Bool.false
