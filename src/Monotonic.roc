module [Monotonic, withCapacity, alloc, set, append, sublist, clear]

Monotonic a := {
    buf : List a,
    capacity : U64,
}

## Create a Monotonic buffer with the given initial capacity.
withCapacity : U64 -> Monotonic a
withCapacity = \n -> @Monotonic { buf: List.withCapacity n, capacity: n }

## Allocate room for an additional N items, initially set to ZERO.
alloc : Monotonic a, U64, a -> Result (U64, Monotonic a) [OutOfMemory]
alloc = \@Monotonic mono, n, zero ->
    if (List.len mono.buf) + n > mono.capacity then
        # enlarge list by (at least) current capacity. Roc doesn't
        # give us a way to query the actual capacity, so we just
        # record a doubling. Also, surprisingly, List.reserve claims
        # it can never fail.
        buf_1 = List.reserve mono.buf mono.capacity
        alloc (@Monotonic { mono & buf: buf_1, capacity: 2 * mono.capacity }) n zero
    else
        # List.len here gives the index of the first element in the
        # newly allocated block.
        Ok (
            List.len mono.buf,
            @Monotonic
                { mono &
                    buf: List.concat mono.buf (List.repeat zero n),
                },
        )

## Set the element at the given index.
set : Monotonic a, U64, a -> Monotonic a
set = \@Monotonic mono, i, x -> @Monotonic { mono & buf: List.set mono.buf i x }

## Append a single item to the end of the buffer.
append : Monotonic a, a -> Result (Monotonic a) [OutOfMemory]
append = \mono, x ->
    Result.try (alloc mono 1 x) \(p, mono_1) ->
        Ok (set mono_1 p x)

sublist : Monotonic a, { start : U64, len : U64 } -> List a
sublist = \@Monotonic mono, { start, len } ->
    List.sublist mono.buf { start, len }

## Clear the buffer, retaining excess capacity.
clear : Monotonic a -> Monotonic a
clear = \@Monotonic mono ->
    # TODO: confirm that this simply returns a seamless slice to the
    # existing buffer.
    @Monotonic { mono & buf: List.takeFirst mono.buf 0 }

expect
    l =
        List.repeat 16 123
        |> List.takeFirst 0
    0 == List.len l

expect
    mono = Monotonic.withCapacity 16
    (p0, _mono1) = alloc mono 1 0 |> Result.withDefault (999u64, mono)
    0 == p0

expect
    mono = Monotonic.withCapacity 16
    (_p0, mono1) = alloc mono 1 0 |> Result.withDefault (999u64, mono)
    (p1, _mono2) = alloc mono1 1 0 |> Result.withDefault (999u64, mono)
    1 == p1

expect
    mono = Monotonic.withCapacity 16
    (p, _mono1) = alloc mono 16 0 |> Result.withDefault (999u64, mono)
    0 == p

expect
    mono = Monotonic.withCapacity 16
    when alloc mono 1 0 is
        Ok (p0, mono_1) ->
            mono_2 = mono_1 |> set p0 123u64
            res = sublist mono_2 { start: 0, len: 1 }
            [123] == res

        _ -> Bool.false

expect
    mono = Monotonic.withCapacity 16
    when append mono 123 is
        Ok mono_1 ->
            [123] == sublist mono_1 { start: 0, len: 1 }

        _ -> Bool.false
