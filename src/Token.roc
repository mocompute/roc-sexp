module [
    Token,
    TokenError,
    next,
    State,
    stateInit,
]

# TODO: make this a module parameter
# https://github.com/roc-lang/roc/tree/main/crates/cli/tests/test-projects/module_params
bufCapacity = 1024



Token : [TOpenRound, TCloseRound, TSymbol Str, TString Str, TNumber Dec]
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
stateResetBuf = \s -> { s & bufIndex: 0 }
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
    Result.try res \r -> Ok (Str.releaseExcessCapacity r, st |> stateResetBuf)

isWhitespace = \c ->
    when c is
        ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false

isDigit = \c -> c >= '0' && c <= '9'

isIdentifierSpecial = \c ->
    when c is
        '_' | '.' | '%' | '*' -> Bool.true
        _ -> Bool.false

isIdentifierStart = \c ->
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || isIdentifierSpecial c

isIdentifierMiddle = \c -> isIdentifierStart c || isDigit c

next : State -> Result ((Token, U64), State) TokenError
next = \s ->
    { source, index } = s
    when List.get source index is
        Ok '(' -> Ok ((TOpenRound, index), { s & index: index + 1 })
        Ok ')' -> Ok ((TCloseRound, index), { s & index: index + 1 })
        Ok '"' -> stringStart { s & start: index + 1, index: index + 1 }
        Ok c ->
            if isWhitespace c then
                next { s & index: index + 1 }
            else if isIdentifierStart c then
                identifierStart ({ s & start: index } |> stateAppendChar c |> stateIncrIndex)
            else if isDigit c then
                numericStart ({ s & start: index } |> stateAppendChar c |> stateIncrIndex)
            else
                Err (Eof 0)

        Err OutOfBounds -> Err (Eof index)

# -- numeric -------------------------------------------------------
#
# symbols which start with a digit are processed until whitespace or a
# round bracket, and passed to Str.toDec to convert to 128-bit
# decimal.

numericStart : State -> Result ((Token, U64), State) TokenError
numericStart = \s ->
    { source, index } = s

    returnNumber = \st ->
        when stateBufToString st is
            Ok (str, state) ->
                when Str.toDec str is
                    Ok dec ->
                        Ok ((TNumber dec, state.start), state |> stateIncrIndex)

                    Err _ -> Err (InvalidToken state.start)

            Err err -> err

    when List.get source index is
        Err OutOfBounds ->
            # backtrack so next\ gets the correct index for an Eof
            returnNumber (s |> stateDecrIndex)

        Err _ ->
            Err (Eof index)

        Ok c ->
            if isWhitespace c || c == '(' || c == ')' then
                # backtrack so next\ can process the whitespace or
                # TCloseRound
                returnNumber (s |> stateDecrIndex)
            else
                numericStart (s |> stateAppendChar c |> stateIncrIndex)

# -- identifier ----------------------------------------------------

identifierStart : State -> Result ((Token, U64), State) TokenError
identifierStart = \s ->
    { source, index } = s

    returnTSymbol = \st ->
        when stateBufToString st is
            Ok (str, state) -> Ok ((TSymbol str, state.start), state |> stateIncrIndex)
            Err err -> err

    when List.get source index is
        Err OutOfBounds ->
            # backtrack so next\ gets the correct index for an Eof
            returnTSymbol (s |> stateDecrIndex)

        Err _ ->
            Err (Eof index)

        Ok c ->
            if isIdentifierMiddle c then
                identifierStart (s |> stateAppendChar c |> stateIncrIndex)
            else if isWhitespace c || c == '(' || c == ')' then
                # backtrack so next\ can process the whitespace or
                # TCloseRound
                returnTSymbol (s |> stateDecrIndex)
            else
                Err (InvalidToken index)

# -- string --------------------------------------------------------

stringStart : State -> Result ((Token, U64), State) TokenError
stringStart = \s ->
    { source, index } = s

    returnString = \st ->
        when stateBufToString st is
            Ok (str, state) -> Ok ((TString str, state.start), state |> stateIncrIndex)
            Err err -> err

    when List.get source index is
        Ok '"' -> returnString s
        Ok '\\' -> stringBackslash { s & index: (index + 1) }
        Ok c -> stringStart (s |> stateAppendChar c |> stateIncrIndex)
        Err OutOfBounds -> Err (Eof index)

stringBackslash = \s ->
    { source, index } = s
    when List.get source index is
        Ok '\\' -> stringStart (s |> stateAppendChar '\\' |> stateIncrIndex)
        Ok c -> stringStart (s |> stateAppendChar c |> stateIncrIndex)
        Err OutOfBounds -> Err (Eof index)

# -- tests ---------------------------------------------------------

WalkState : { tokState : State, break : [None, Mismatch, Error] }

check_ : WalkState, TokenOrError -> [Break WalkState, Continue WalkState]
check_ = \st, expected ->
    { tokState } = st

    when next tokState is
        Ok ((tok, _), nextTokState) ->
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

        Err err ->
            when expected is
                Ok good ->
                    dbg (expectedVsActual good err)
                    Break { st & break: Mismatch }

                Err expectError ->
                    if err == expectError then
                        Continue st
                    else
                        dbg (expectedVsActual expectError err)
                        Break { st & break: Mismatch }

expectedVsActual = \expected, actual ->
    "\nexpected: $(Inspect.toStr expected)\nactual:   $(Inspect.toStr actual)"

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
    good = [Ok TOpenRound, Ok TCloseRound, Err (Eof 2)]
    checkTokens source good

expect
    source = "abcdef"
    good = [Ok (TSymbol "abcdef"), Err (Eof 6)]
    checkTokens source good

expect
    source = "(abcdef)"
    good = [Ok TOpenRound, Ok (TSymbol "abcdef"), Ok TCloseRound, Err (Eof 8)]
    checkTokens source good

expect
    source = "(\"string 1\" \"string 2\")"
    good = [Ok TOpenRound, Ok (TString "string 1"), Ok (TString "string 2"), Ok TCloseRound, Err (Eof 23)]
    checkTokens source good

expect
    source = "\"hello back\\\\slash\""
    good = [Ok (TString "hello back\\slash"), Err (Eof 19)]
    checkTokens source good

expect
    source = "123"
    good = [Ok (TNumber 123), Err (Eof 3)]
    checkTokens source good

expect
    source = "123.456789"
    good = [Ok (TNumber 123.456789), Err (Eof 10)]
    checkTokens source good

expect
    source = "123xyz"
    good = [Err (InvalidToken 0)]
    checkTokens source good
