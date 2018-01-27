
# the following methods agglomerate pattern-repl pairs.
const TFC = Union{TF,Char}

# if pattern a is found, pattern b will not be used
is_shadowing(a::Char, b::Char) = a == b
is_shadowing(a::Char, b::String) = length(b) == 1 && a == b[1]
is_shadowing(a::String, b::Char) = length(a) == 1 && a[1] == b
is_shadowing(a::String, b::String) = a == b
function is_shadowing(a::Regex, b::Char)
    m = match(a, string(b))
    m != nothing && m.match == string(b)
end
function is_shadowing(a::Regex, b::String)
    m = match(a, string(b))
    m != nothing && m.match == b
end
is_shadowing(a::Callable, b::Char) = a(b)
is_shadowing(a::Callable, b::String) = length(b) == 1 && a(b[1])
is_shadowing(a::TFC, b::TFC) = a == b

function remove_all_shadowed!(pairs::Vector{<:Pair{<:TFC,<:TS}})
    for i = length(pairs):-1:1
        if any(is_shadowing.(first.(pairs[1:i-1]), first(pairs[i])))
            deleteat!(pairs, i)
        end
    end
    pairs
end

isless_pattern(a::Pair, b::Pair) = isless_pattern(first(a), first(b))

isless_pattern(a::String, b::String) = length(b) < length(a)
isless_pattern(a::String, ::Char) = 1 < length(a)
isless_pattern(::Char, b::String) = length(b) < 1
isless_pattern(a::Union{Char,String}, ::TFC) = true
isless_pattern(a::TFC, b::String) = false
isless_pattern(a::TFC, b::TFC) = false

function sortpatterns!(pairs::AbstractVector)
    sort!(pairs, lt=isless_pattern, alg=Base.DEFAULT_STABLE)
end

# if regex is of the form "aaa(|bbb)*" replace it with array of strings
function expand_pattern(re::Regex)
    s = re.pattern
    words = Union{String,Regex}[]
    io = IOBuffer()
    k = 1
    i = 1
    while !done(s, i)
        c, i = next(s, i)
        if c == '\\'
            done(s, i) && error("regex ends with \\")
            c, i = next(s, i)
            write(io, c)
        elseif c == '|'
            push!(words, String(take!(io)))
            k = i
        elseif c in "[|+*^\\\$"
            push!(words, Regex(s[k:end]))
            k = 0
            break
        else
            write(io, c)
        end
    end
    if k > 0
        push!(words, String(take!(io)))
    end
    n = length(words) - (k == 0)
    sortpatterns!(view(words, 1:n))
    unique(words)
end

expand_pattern(s::AbstractString) = [s]
expand_pattern(f::Base.EqualTo{Char}) = [f.x]
expand_pattern(f::Base.OccursIn) = unique([c for c in f.x])
expand_pattern(x) = [x]

function expand_pairs(pairs::AbstractVector{<:Pair})
    res = Pair{<:TFC,<:TS}[]
    for p in pairs
        append!(res, Pair.(expand_pattern(p.first), p.second))
    end
    remove_all_shadowed!(res)
    sortpatterns!(res)
    unique(res)
end

# Condense strings and characters to a single regex.
# The repl is a mapping of patterns to replacement strings.
# it is assumed, that the pairs are sorted with the
# longer strings before the 1-character strings before
# before regexes, functions, and empty string.
#
function condense_pairs(pairs::AbstractVector{<:Pair})
    n = findlast(p->(p.first isa Union{Char,String}), pairs)
    ( n == nothing || n <= 1 ) && return pairs
    ps = view(pairs, 1:n)
    dict = Dict{String,Any}(Pair.(string.(first.(ps)), last.(ps)))
    pout = Pair[]
    fdict(c) = dict[string(c)]
    if length(ps[1].first) > 1 || length(ps[n].first) < 1
        io = IOBuffer()
        frst = true
        for p in ps
            frst || write(io, '|')
            escape_string(io, string(p.first), "\\\$^[*+()/{}|")
            frst = false
        end
        push!(pout, Pair(Regex(String(take!(io))), fdict))

    elseif length(pairs[n].first) == 1
        push!(pout, Pair(occursin(join(first.(ps))), fdict))
    end
    append!(pout, pairs[n+1:end])
end

