module [Token, next]

Token : [OpenRound, CloseRound, Symbol Str, String Str, Number Dec]

TokenError : [
    InvalidToken U64,
    Eof U64,
    BadUtf8 U64,
]

State : { start : U64 }

initialState = { start: 0 }

next : List U8, U64 -> Result (Token, List U8) TokenError
next = \bs, index -> next_ initialState bs index

next_ : State, List U8, U64 -> Result (Token, List U8) TokenError
next_ = \state, bs, index ->
    when List.get bs index is
        Ok '(' -> Ok (OpenRound, bs)
        Ok ')' -> Ok (CloseRound, bs)
        Ok '"' -> stringStart { state & start: index + 1 } bs (index + 1)
        Ok ' ' | Ok '\n' | Ok '\r' | Ok '\t' -> next_ state bs (index + 1)
        Ok _ -> Err (Eof 0)
        Err OutOfBounds -> Err (Eof index)

stringStart : State, List U8, U64 -> Result (Token, List U8) TokenError
stringStart = \state, bs, index ->
    when List.get bs index is
        Ok '"' ->
            { start } = state
            len = index - start
            res =
                List.sublist bs { start, len }
                |> Str.fromUtf8
                |> Result.mapErr \_ -> Err (BadUtf8 index)
            Result.try res \r -> Ok (String r, bs)

        Ok _ -> stringStart state bs (index + 1)
        Err OutOfBounds -> Err (Eof index)


expect
    source = Str.toUtf8 "("
    when next source 0 is
        Ok (OpenRound, _) -> Bool.true
        _ -> Bool.false

expect
    source = Str.toUtf8 "\"hello\""
    res = next source 0
    when res is
        Ok (String "hello", _) -> Bool.true
        _ ->
            dbg res
            Bool.false
