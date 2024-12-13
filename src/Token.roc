module [Token, next]

# TODO: make this a module parameter
# https://github.com/roc-lang/roc/tree/main/crates/cli/tests/test-projects/module_params
bufCapacity = 1024

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

stateInit = \source -> { source, index: 0, start: 0, buf: List.repeat 0 bufCapacity, bufIndex: 0 }
stateResetBuf = \s -> { s & buf: listClear s.buf, bufIndex: 0 }
stateIncrIndex = \s -> { s & index: s.index + 1 }
stateAppendChar = \s, c -> { s &
        buf: List.set s.buf s.bufIndex c,
        bufIndex: s.bufIndex + 1,
    }

listClear = \list ->
    f = \l, len, i ->
        if i >= len then
            l
        else
            f (List.set l i 0) len (i + 1)

    f list (List.len list) 0

next : State -> Result (Token, State) TokenError
next = \s ->
    { source, index } = s
    when List.get source index is
        Ok '(' -> Ok (OpenRound, { s & index: index + 1 })
        Ok ')' -> Ok (CloseRound, { s & index: index + 1 })
        Ok '"' -> stringStart { s & start: index + 1, index: index + 1 }
        Ok ' ' | Ok '\n' | Ok '\r' | Ok '\t' -> next { s & index: index + 1 }
        Ok _ -> Err (Eof 0)
        Err OutOfBounds -> Err (Eof index)

stringStart : State -> Result (Token, State) TokenError
stringStart = \s ->
    { source, index } = s
    when List.get source index is
        Ok '"' ->
            # end of string
            res =
                s.buf
                |> List.takeFirst s.bufIndex
                |> Str.fromUtf8
                |> Result.mapErr \_ -> Err (BadUtf8 index)
            Result.try res \r -> Ok (String r, s |> stateResetBuf |> stateIncrIndex)

        Ok '\\' -> stringBackslash { s & index: (index + 1) }
        Ok c -> stringStart (s |> stateAppendChar c |> stateIncrIndex)
        Err OutOfBounds -> Err (Eof index)

stringBackslash = \s ->
    { source, index } = s
    when List.get source index is
        Ok '\\' -> stringStart (s |> stateAppendChar '\\' |> stateIncrIndex)
        Ok c -> stringStart (s |> stateAppendChar c |> stateIncrIndex)
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

    s = stateInit (Str.toUtf8 source)

    walkState = { tokState: s, break: None }
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

    s = stateInit (Str.toUtf8 source)

    walkState = { tokState: s, break: None }
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

    s = stateInit (Str.toUtf8 source)
    walkState = { tokState: s, break: None }
    res = List.walkUntil
        good
        walkState
        check_

    when res.break is
        None -> Bool.true
        _ -> Bool.false
