
export replace_kcon

# specialized search function for Char
function findfunc(p::Pair{Char})
    (s::AbstractString, i::Int) -> begin
        r = findnext(equalto(p.first), s, i)
        r, r, ind
    end
end

# specialized search function for collections of Char
function findfunc(p::Pair{<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}})
    (s::AbstractString, i::Int) -> begin
        r = findnext(occursin(p.first), s, i)
        r, r, p
    end
end

# search function for compound single char functions
function findfunc(p::Pair{Nothing,<:Vector{<:Pair}})
    pairs = p.second
    (s::AbstractString, i::Int) -> begin
        ends = endof(s)
        n = length(pairs)
        while i <= ends
            c = s[i]
            j = 1
            while j <= n
                pp = pairs[j]
                pf = pp.first
                if pf isa Char
                    c == pf && return i, i, pp
                else
                    pf(c) && return i, i, pp
                end
                j += 1
            end
            i = nextind(s, i)
        end
        0, 0, p
    end
end

# search function for all other cases (p.first can be passed to findnext)
function findfunc(p::Pair)
    (s::AbstractString, i::Int) -> begin
        r = findnext(p.first, s, i)
        first(r), last(r), p
    end
end

# accumulate in st/d - discard duplicate patterns
function pushrx!(c::String, repl::Any, st::Vector{String}, d::Dict{String,Any})
    cs = string(c)
    if !haskey(d, cs)
        d[cs] = repl
        push!(st, cs)
        return true
    end
    false
end

# push pattern-repl pair depending on type of pattern
function pushpat!(pair::Pair{Char}, pairs, st, d, singles)
    pushrx!(string(pair.first), pair.second, st, d) &&
    push!(singles, pair)
end
function pushpat!(pair::Pair{<:AbstractString}, pairs, st, d, singles)
    pf = pair.first
    ps = pair.second
    pushrx!(pf, pair.second, st, d) && length(pf) == 1 && push!(singles, pf[1]=>ps)
end
function pushpat!(pair::Pair{<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}}, pairs, st, d, singles)
    
    pat = pair.first
    ps = pair.second
    if length(pat) == 1
        pushpat!(first(pat)=>ps, pairs, st, d, singles)
    else
        for c in pat
            pushrx!(string(c), ps, st, d) &&
            push!(singles, c=>ps)
        end
    end
end
function pushpat!(pair::Pair{<:Base.EqualTo}, pairs, st, d, singles)
    pushpat!(pair.first.x=>pair.second, pairs, st, d, singles)
end
function pushpat!(pair::Pair{<:Base.OccursIn}, pairs, st, d, singles)
    for c in pair.first.x
        pushpat!(c=>pair.second, pairs, st, d, singles)
    end
end
function pushpat!(pair::Pair{<:Function}, pairs, st, di, singles)
    push!(singles, pair)
end
pushpat!(pair::Pair, pairs, st, d, singles) = push!(pairs, pair)

# post process replacement (if not a SubstitutionString)
function _postprocessor(d::Dict{String,Any})
    (s::AbstractString) -> _postprocess(s, d[s])
end

_postprocess(s::AbstractString, ds::Union{AbstractString,Char}) = ds
_postprocess(s::AbstractString, ds::Function) = ds(s)
_postprocess(s::AbstractString, ds) = string(ds)

# extract all String, Char, and collection of Char patterns
# and combine to one regex + replacement function
# all others remain
function build_regex(pat_repls::NTuple{N,Pair}) where N
    pairs = Vector{Pair}()
    st = Vector{String}()
    d = Dict{String,Any}()
    singles = Vector{Pair}()

    for i = 1:N
        pair = pat_repls[i]
        #= TODO the following lines create Segmentation fault
        # Julia Version 0.7.0-DEV.3354
        # Commit 9b5eed2b6c* (2018-01-09 08:03 UTC)
        if pair.second isa Char
            pair = Pair(pair.first, pair.second)
        end
        =#
        pushpat!(pair, pairs, st, d, singles)
    end

    if all(p->p.first isa Char, singles)
        empty!(singles)
    else
        sc = string.(filter(c->c isa Char, first.(singles)))
        setdiff!(st, sc)
        delete!.(d, sc)
    end

    if length(singles) > 0
        pushfirst!(pairs, nothing=>singles)
    end

    if length(st) == 1
        str = st[1]
        pushfirst!(pairs, str=>d[str])
    elseif length(st) > 1
        buf = IOBuffer()
        sort!(st, lt=(a,b)->sizeof(a)>sizeof(b))
        frst = true
        for pat in st
            frst || print(buf, '|')
            escape_string(buf, pat, "[\\^\$.|?*+()")
            frst = false
        end
        rs = String(take!(buf))
        pushfirst!(pairs, Regex(rs)=>_postprocessor(d))
    end

    pairs
end

# find the first of a list of patterns.
# if several match at the same start position prefer the longer one
# if also the length is equal, prefer the first in list
# return index-range of the successful search and the pair
# if no match is found, return 0, 0 and some pair
# The search moves forward through s exactly once for each pair
# If no match is found for a pair, it is removed from the list
#
function findnextall(fun::Vector{Function}, prp::Vector{Pair}, rv1::Vector{Int}, rv2::Vector{Int}, s::AbstractString, start::Int)

    n = length(fun)
    if n > 0
        # new find for all matches that started before start (outdated)
        for i = 1:n
            if rv1[i] < start #overlap
                # new search starting one after
                rv1[i], rv2[i], prp[i] = fun[i](s, start)
            end
        end
        # remove all pairs without matching patterns
        for i = n:-1:1
            if rv1[i] <= 0
                deleteat!(fun, i)
                deleteat!(prp, i)
                deleteat!(rv1, i)
                deleteat!(rv2, i)
            end
        end
    end

    n = length(fun)
    if n == 0
        0, 0, (""=>"")
    else
        minstart = rv1[1]
        minend = rv2[1]
        minp = prp[1]
        for i = 2:n
            r1 = rv1[i]
            r2 = rv2[i]
            if r1 < minstart || ( r1 == minstart && r2 > minend)
                minstart = r1
                minend = r2
                minp = prp[i]
            end
        end
        # next match at minstart:minend for pat_repl with original index minp

        minstart, minend, minp
    end
end

mutable struct ReplaceCache
    state::Int  # 0: invalid, 1 valid pat_repls, pairs, fun
    pat_repls::NTuple{N,Pair} where N
    pairs::Vector{Pair}
    fun::Vector{Function}  # pending functions to call
    ReplaceCache() = new(0)
end

reset!(c::ReplaceCache) = c.state = 0

# cover the multiple pairs case
function replace_kcon(str::String, pat_repls::Pair...; count::Integer=typemax(Int), cache::ReplaceCache=ReplaceCache())

    if count == 0 || length(pat_repls) == 0
        return str
    end
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))

    if cache.state == 0
        # transform and condense pat_repl patterns to pairs
        cache.pat_repls = pat_repls
        cache.pairs = build_regex(pat_repls)
        cache.fun = Vector{Function}(findfunc.(cache.pairs))
        cache.state = 1
    end

    n = length(cache.pairs)
    if n <= 1
        n == 0 && return str
        pair = cache.pairs[1]
        if pair.first != nothing
            return replace(str, pair, count=count) # special for one pair
        end
    end

    # the following 4 vectors are kept parallel (corresponding elements)
    fun = copy(cache.fun)   # pending function calls
    prp = Vector{Pair}(uninitialized, n) # pattern-repl-pair being processed
    rv1 = zeros(Int, n)     # next match start - 0 means no more match
    rv2 = zeros(Int, n)     # next match start - 0 means no more match

    eos = endof(str)
    i = a = start(str)
    j, k, ind = findnextall(fun, prp, rv1, rv2, str, i)
    out = IOBuffer(StringVector(sizeof(str)*12÷10), true, true)
    out.size = 0
    out.ptr = 1
    n = 1
    while j != 0 && n <= count
        if i == a || i <= k
            unsafe_write(out, pointer(str, i), UInt(j-i))
            pattern, repl = ind
            _replace(out, repl, str, j:k, pattern)
        end
        if k < j
            i = j
            j > eos && break
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        j, k, ind = findnextall(fun, prp, rv1, rv2, str, k)
        n += 1
    end
    write(out, SubString(str, i))
    String(take!(out))
end

#=
# The following is dedicated to single pat-repl.
# replace Char with function that compares with this Char
predicatepair(p::Pair{Char}) = equalto(p.first) => p.second

# replace collection of Char with function that checks occurrence in this collection
function predicatepair(p::Pair{<:Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}})
    occursin(p.first) => p.second
end
predicatepair(p::Pair) = p

=#
