# buffer-reuse.roc
#
# roc test buffer-reuse.roc succeeds
# roc test --optimize buffer-reuse.roc fails
#
# Failure is that actual string is "\0\0\0\0" when it should be
# "abcd". Seems likely that when returing the string `r` from
# `stateBufToString`, it is still referencing the underlying buffer,
# which is being cleared in the same expression.

module []

bufCapacity = 1024

State : {
    buf : List U8,
    bufIndex : U64,
}

stateInit = \{} -> { buf: List.repeat 0 bufCapacity, bufIndex: 0 }
stateResetBuf = \s -> { s & buf: listClear s.buf, bufIndex: 0 }
stateAppendChar = \s, c -> { s &
        buf: List.set s.buf s.bufIndex c,
        bufIndex: s.bufIndex + 1,
    }

stateBufToString = \st ->
    res =
        st.buf
        |> List.takeFirst st.bufIndex
        |> Str.fromUtf8
    Result.try res \r -> Ok (r, st |> stateResetBuf)

    # NOTE: replacing the above line with this one causes the test to
    # succeed, even with the call to `listClear`
    # Result.try res \r -> Ok (Str.releaseExcessCapacity r, st |> stateResetBuf)

# NOTE: using this version of listClear causes the bug to appear
listClear = \list ->
    f = \l, sz, i -> if i < sz then f (List.set l i 0) sz (i + 1) else l
    f list (List.len list) 0

# NOTE: replace with noop version and bug does not appear
# listClear = \list -> list

step1 : State -> Result (Str, State) _
step1 = \st ->
    st
    |> stateAppendChar 'a'
    |> stateAppendChar 'b'
    |> stateAppendChar 'c'
    |> stateAppendChar 'd'
    |> stateBufToString

expect
    when step1 (stateInit {}) is
        Ok (str, _) ->
            dbg str
            str == "abcd"
        Err _ ->
            0 == 1
