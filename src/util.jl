
_replace(io, repl, str, r, pattern) = print(io, repl)
_replace(io, repl::Function, str, r, pattern) =
    print(io, repl(SubString(str, first(r), last(r))))

replace_j(str::String, pat_repl::Pair{Char}, count::Integer=typemax(Int)) =
    replace_j(str, equalto(first(pat_repl)) => last(pat_repl), count)
replace_j(str::String, pat_repl::Pair{<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}}, count::Integer=typemax(Int)) =
    replace_j(str, occursin(first(pat_repl)) => last(pat_repl), count)

function replace_j(str::String, pat_repl::Pair)
    count::Integer=typemax(Int)
    pattern, repl = pat_repl
    count == 0 && return str
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    n = 1
    e = endof(str)
    i = a = start(str)
    r = findnext(pattern,str,i)
    j, k = first(r), last(r)
    out = IOBuffer(StringVector(floor(Int, 1.2sizeof(str))), true, true)
    out.size = 0
    out.ptr = 1
    while j != 0
        if i == a || i <= k
            unsafe_write(out, pointer(str, i), UInt(j-i))
            _replace(out, repl, str, r, pattern)
        end
        if k < j
            i = j
            j > e && break
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        r = findnext(pattern,str,k)
        r == 0:-1 || n == count && break
        j, k = first(r), last(r)
        n += 1
    end
    write(out, SubString(str,i))
    String(take!(out))
end

"""
    replace(s::AbstractString, pat=>r; [count::Integer])

Search for the given pattern `pat` in `s`, and replace each occurrence with `r`.
If `count` is provided, replace at most `count` occurrences.
`pat` may be a single character, a vector or a set of characters, a string,
or a regular expression. If `r`
is a function, each occurrence is replaced with `r(s)` where `s` is the matched substring.
If `pat` is a regular expression and `r` is a `SubstitutionString`, then capture group
references in `r` are replaced with the corresponding matched text.
To remove instances of `pat` from `string`, set `r` to the empty `String` (`""`).

# Examples
```jldoctest
julia> replace("Python is a programming language.", "Python" => "Julia")
"Julia is a programming language."

julia> replace("The quick foxes run quickly.", "quick" => "slow", count=1)
"The slow foxes run quickly."

julia> replace("The quick foxes run quickly.", "quick" => "", count=1)
"The  foxes run quickly."
```
"""
replace_j(s::AbstractString, pat_f::Pair, count=typemax(Int)) =
    replace_j(String(s), pat_f)

# TODO: allow transform as the first argument to replace?

