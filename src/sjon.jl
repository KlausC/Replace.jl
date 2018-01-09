import Base: _replace, StringVector

# replace Char with function that compares with this Char
predicatepair(p::Pair{Char}) = equalto(p.first) => p.second

# replace collection of Char with function that checks occurrence in this collection
function predicatepair(p::Pair{<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}})
    occursin(p.first) => p.second
end
predicatepair(p::Pair) = p


# find the first of a list of patterns.
# if several match at the same start position prefer the longer one
# if also the length is equal, prefer the first in list
# return index-range of the successful search and the pair
function findnextall!(pairs::NTuple{N,Pair}, s::AbstractString, st::Integer, rangecache) where N
    minstart=minend=endof(s)+1
    nmin=0
    for n = 1:N
        p = pairs[n]
        fr = first(rangecache[n])
        if fr ≥ st
            r = rangecache[n] # re-use previous search result
            last(r) == 0 && continue # not found
        else
            r = rangecache[n] = findnext(p.first, s, st)
            fr = first(r)
        end
        if fr > 0
            e = last(r)
            if fr < minstart || (fr == minstart && e > minend)
                minstart, minend, nmin = fr, e, n
            end
        else # not found
            rangecache[n] = endof(s)+1:0
        end
    end
    return minstart:minend, nmin
end

# cover the multiple pairs case
function replace_sjon(str::String, pat_repls_::Pair...; count::Integer=typemax(Int))
    if count == 0 || isempty(pat_repls_)
        return str
    end
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    pat_repls = predicatepair.(pat_repls_)
    n = 1
    e = endof(str)
    i = a = start(str)
    rangecache = [0:0 for i=1:length(pat_repls)]
    r, npat = findnextall!(pat_repls, str, i, rangecache)
    j, k = first(r), last(r)
    out = IOBuffer(StringVector(floor(Int, 1.2sizeof(str))), true, true)
    out.size = 0
    out.ptr = 1
    while npat > 0 && n ≤ count
        if i == a || i <= k
            unsafe_write(out, pointer(str, i), UInt(j-i))
            pattern, repl = pat_repls[npat]
            _replace(out, repl, str, r, pattern)
        end
        if k < j
            i = j
            j > e && break
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        r, npat = findnextall!(pat_repls, str, k, rangecache)
        j, k = first(r), last(r)
        n += 1
    end
    write(out, SubString(str, i))
    String(take!(out))
end

