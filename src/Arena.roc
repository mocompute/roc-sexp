module [Arena, create, alloc, set, append, next, sublist, reset]

Arena a := {
    buf : List a,
    capacity : U64,
    index : U64,
}

create : U64, a -> Arena a
create = \n, x -> @Arena { buf: List.repeat x n, capacity: n, index: 0 }

alloc : Arena a, U64 -> Result (U64, Arena a) [OutOfMemory]
alloc = \@Arena arena, n ->
    if arena.index + n > arena.capacity then
        Err OutOfMemory
    else
        Ok (arena.index, @Arena { arena & index: arena.index + n })

set : Arena a, U64, a -> Arena a
set = \@Arena arena, i, x -> @Arena { arena & buf: List.set arena.buf i x }

append : Arena a, a -> Result (Arena a) [OutOfMemory]
append = \arena, x ->
    Result.try (alloc arena 1) \(p, arena_1) ->
        Ok (set arena_1 p x)

next : Arena a -> U64
next = \@Arena arena -> arena.index

sublist : Arena a, { start : U64, len : U64 } -> List a
sublist = \@Arena arena, { start, len } ->
    List.sublist arena.buf { start, len }

reset : Arena a -> Arena a
reset = \@Arena arena -> @Arena { arena & index: 0 }

expect
    arena = Arena.create 16 0
    (p0, _arena1) = alloc arena 1 |> Result.withDefault (999u64, arena)
    0 == p0

expect
    arena = Arena.create 16 0
    (_p0, arena1) = alloc arena 1 |> Result.withDefault (999u64, arena)
    (p1, _arena2) = alloc arena1 1 |> Result.withDefault (999u64, arena)
    1 == p1

expect
    arena = Arena.create 16 0
    (p, arena1) = alloc arena 16 |> Result.withDefault (999u64, arena)
    0 == p && (alloc arena1 1 |> Result.isErr)

expect
    arena = Arena.create 16 0
    alloc arena 17 |> Result.isErr

expect
    arena = Arena.create 16 0
    when alloc arena 1 is
        Ok (p0, arena_1) ->
            arena_2 = arena_1 |> set p0 123u64
            res = sublist arena_2 { start: 0, len: 1 }
            [123] == res

        _ -> Bool.false

expect
    arena = Arena.create 16 0
    when append arena 123 is
        Ok arena_1 ->
            [123] == sublist arena_1 { start: 0, len: 1 }

        _ -> Bool.false
