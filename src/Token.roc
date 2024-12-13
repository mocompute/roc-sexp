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
stateDecrIndex = \s -> { s & index: s.index - 1 }
stateAppendChar = \s, c -> { s &
        buf: List.set s.buf s.bufIndex c,
        bufIndex: s.bufIndex + 1,
    }

stateBufToString = \st ->
    res =
        st.buf
        |> List.takeFirst st.bufIndex
        |> Str.fromUtf8
        |> Result.mapErr \_ -> Err (BadUtf8 st.start)
    Result.try res \r -> Ok (r, st |> stateResetBuf)

## Reset contents of a list to all 0 in-place
listClear = \list ->
    f = \l, sz, i -> if i < sz then f (List.set l i 0) sz (i + 1) else l
    f list (List.len list) 0

isWhitespace = \c ->
    when c is
        ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false

isDigit = \c -> c >= '0' && c <= '9'
# isDecimalPoint = \c -> c == '.'

isIdentifierSpecial = \c ->
    when c is
        '_' | '.' | '%' | '*' -> Bool.true
        _ -> Bool.false

isIdentifierStart = \c ->
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || isIdentifierSpecial c

isIdentifierMiddle = \c -> isIdentifierStart c || isDigit c

next : State -> Result (Token, State) TokenError
next = \s ->
    { source, index } = s
    when List.get source index is
        Ok '(' -> Ok (OpenRound, { s & index: index + 1 })
        Ok ')' -> Ok (CloseRound, { s & index: index + 1 })
        Ok '"' -> stringStart { s & start: index + 1, index: index + 1 }
        Ok ' ' | Ok '\n' | Ok '\r' | Ok '\t' -> next { s & index: index + 1 }
        Ok c ->
            if isIdentifierStart c then
                identifierStart ({ s & start: index } |> stateAppendChar c |> stateIncrIndex)
            else
                Err (Eof 0)

        Err OutOfBounds -> Err (Eof index)

identifierStart : State -> Result (Token, State) TokenError
identifierStart = \s ->
    { source, index } = s

    returnSymbol = \st ->
        when stateBufToString st is
            Ok (str, state) -> Ok (Symbol str, state |> stateIncrIndex)
            Err err -> Err err

    when List.get source index is
        Err OutOfBounds ->
            # backtrack so next\ gets the correct index for an Eof
            returnSymbol (s |> stateDecrIndex)

        Err _ ->
            Err (Eof index)

        Ok c ->
            if isIdentifierMiddle c then
                identifierStart (s |> stateAppendChar c |> stateIncrIndex)
            else if isWhitespace c || c == ')' then
                # backtrack so next\ gets to process the whitespace or
                # CloseRound
                returnSymbol (s |> stateDecrIndex)
            else
                Err (InvalidToken index)

stringStart : State -> Result (Token, State) TokenError
stringStart = \s ->
    { source, index } = s

    returnString = \st ->
        when stateBufToString st  is
            Ok (str, state) -> Ok (String str, state |> stateIncrIndex)
            Err err -> Err err

    when List.get source index is
        Ok '"' ->
            returnString s
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
                        dbg (expectedVsActual good tok)
                        Break { st & break: Mismatch }

                Err err ->
                    dbg (expectedVsActual err tok)
                    Break { st & break: Mismatch }

        Err (Eof index) ->
            when expected is
                Ok good ->
                    dbg (expectedVsActual good (Eof index))
                    Break { st & break: Mismatch }

                Err (Eof expectedIndex) ->
                    if index == expectedIndex then
                        Break st
                    else
                        dbg (expectedVsActual (Eof expectedIndex) (Eof index))
                        Break { st & break: Mismatch }

                Err _ ->
                    Break { st & break: Error }

        Err _ ->
            Break { st & break: Error }

expectedVsActual = \expected, actual ->
    "\nexpected: $(Inspect.toStr expected)\nactual:   $(Inspect.toStr actual)"

#

## Tokenize source and compare against expected.
checkTokens = \source, expected ->
    s = stateInit (Str.toUtf8 source)

    walkState = { tokState: s, break: None }
    res = List.walkUntil
        expected
        walkState
        check_

    when res.break is
        None -> Bool.true
        _ -> Bool.false

expect
    source = "()"
    good = [Ok OpenRound, Ok CloseRound, Err (Eof 2)]
    checkTokens source good

expect
    source = "abcdef"
    good = [Ok (Symbol "abcdef"), Err (Eof 6)]
    checkTokens source good

expect
    source = "(abcdef)"
    good = [Ok OpenRound, Ok (Symbol "abcdef"), Ok CloseRound, Err (Eof 8)]
    checkTokens source good

expect
    source = "(\"string 1\" \"string 2\")"
    good = [Ok OpenRound, Ok (String "string 1"), Ok (String "string 2"), Ok CloseRound, Err (Eof 23)]
    checkTokens source good

expect
    source = "\"hello back\\\\slash\""
    good = [Ok (String "hello back\\slash"), Err (Eof 19)]
    checkTokens source good
