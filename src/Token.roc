module [Token, init, next]

bufCapacity = 1024

Token : [OpenRound, CloseRound, Symbol Str, String Str, Number Dec]

TokenError : [
    InvalidToken U64,
    Eof U64,
    BadUtf8 U64,
]

State : {
    source : List U8,
    index : U64,
    start : U64,
    buf : List U8,
}

init : List U8 -> State
init = \source -> { source, index: 0, start: 0, buf: List.withCapacity bufCapacity }

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
            { start } = state
            len = index - start
            res =
                List.sublist source { start, len }
                |> Str.fromUtf8
                |> Result.mapErr \_ -> Err (BadUtf8 index)
            Result.try res \r -> Ok (String r, state)

        Ok '\\' -> stringBackslash { state & index: (index + 1) }
        Ok c -> stringStart { state & buf: List.append state.buf c, index: index + 1 }
        Err OutOfBounds -> Err (Eof index)

stringBackslash = \state ->
    { source, index } = state
    when List.get source index is
        Ok '\\' -> stringStart { state & buf: List.append state.buf '\\', index: index + 1 }
        Ok c -> stringStart { state & buf: List.append state.buf c, index: index + 1 }
        Err OutOfBounds -> Err (Eof index)

expect
    source = "()"
    good = [OpenRound, CloseRound, Eof 2]

    state = init (Str.toUtf8 source)

    walkState = { tokState: state, break: None }
    res = List.walkUntil
        good
        walkState
        (\st, token ->
            { tokState } = st

            when next tokState is
                Ok (tok, nextTokState) ->
                    if tok == token then
                        Continue { st & tokState: nextTokState }
                    else
                        Break { st & break: Mismatch }

                Err (Eof index) ->
                    if (Eof index) == token then
                        Break st
                    else
                        Break { st & break: Mismatch }

                Err _ ->
                    Break { st & break: Error })

    when res.break is
        None -> Bool.true
        _ -> Bool.false


expect
    source = "\"hello\""
    state = init (Str.toUtf8 source)
    res = next state
    when res is
        Ok (String "hello", _) -> Bool.true
        _ ->
            dbg res
            Bool.false
