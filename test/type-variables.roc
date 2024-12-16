module []

Buffer a := { data: List a }

withCapacity = \n -> @Buffer { data: List.withCapacity n }

add : Buffer a, a -> Buffer a
add = \@Buffer buffer, x -> @Buffer { buffer & data: List.append buffer.data x }

get : Buffer a, U64 -> Result a _
get = \@Buffer buffer, i -> List.get buffer.data i

prealloc : U64 -> List a
prealloc = \n -> List.repeat * n

prealloc2 : U64, ({} -> a) -> List a
prealloc2 = \n, f -> List.repeat (f {}) n

prealloc3 : U64, a -> List a
prealloc3 = \n, x -> List.repeat x n


expect
    buffer = withCapacity 16
    buffer1 = add buffer 123
    Ok 123 == get buffer1 0

expect
    buffer = withCapacity 16
    buffer1 = add buffer "hello"
    Ok "hello" == get buffer1 0
