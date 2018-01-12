
import Base.Callable

const TF = Union{String,Callable,Regex}
const TS = Union{String,Callable,Base.SubstitutionString}

const TIF = Union{Char,AbstractString,Regex,Callable,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}
const TIS = Union{Char,AbstractString,Callable,Base.SubstitutionString}

mutable struct RepAgent
    id::Int             # Index into original list of pattern-replacement pairs 
    pos::Int            # next read position
    r1::Vector{Int}     # pre-stored range.1    
    r2::Vector{Int}     # pre-stored range.end
    RepAgent(id::Int, n::Int) = new(id, n+1, fill(0, n), fill(0, n))
end

function getnext(ra::RepAgent, maps::Vector{<:Pair{<:TF,<:TS}},s::String, ii::Int)
    # dicard all overlapping entries
    pos = ra.pos
    n = length(ra.r1)
    while pos <= n && ra.r1[pos] < ii
        pos += 1
    end
    if pos > n
        _findnext!(maps[ra.id], ra, s, ii)
        # point of type unstability - for big n negligible
        pos = 1
    end
    ra.pos += 1
    ra.r1[pos-1], ra.r2[pos-1]
end

# The following functions should be type-stable
function _findnext!(p::Pair{String}, ra::RepAgent, s::String, ii:Int) 
    n = length(ra.r1)
    i = ii
    for pos = 1:n
        j, k = findnext(first(p), s, i)
        ra.r1[pos], ra.r2[pos] = j, k
        j <= 0 && break
        k = i == ii ? k : i
        i = nextind(s, k)
    end
end

function _findnext!(p::Pair{Regex}, ra::RepAgent, s::String, ii:Int) 
    n = length(ra.r1)
    i = ii
    for pos = 1:n
        j, k = findnext(first(p), s, i)
        ra.r1[pos], ra.r2[pos] = j, k
        j <= 0 && break
        k = i == ii ? k : i
        i = nextind(s, k)
    end
end

function _findnext!(p::Pair{<:Callable}, ra::RepAgent, s::String, ii::Int)
    n = length(ra.r1)
    i = ii
    for pos = 1:n
        j = findnext(first(p), s, i)
        ra.r1[pos], ra.r2[pos] = j, j
        j <= 0 && break
        k = i == ii ? k : i
        i = nextind(s, k)
    end
end

# cover the multiple pairs case
function replace_kcun(str::String, pat_repls::Pair{<:TIF,<:TIS}...)
    count::Integer=typemax(Int)

    if count == 0 || length(pat_repls) == 0
        return str
    end
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    bufl = 10

    n = length(pat_repls)
    # the following 4 vectors are kept parallel (corresponding elements)
    maps::Vector{Pair{<:TF,<:TS}} = predicatepair.(collect(pat_repls))
    prp::Vector{RepAgent} = RepAgent.(1:n, n)
    println("prp = $prp")
    eos = endof(str)
    i::Int = 1
    sta::Int = i
    k::Int = i
    j::Int = 0
    out = IOBuffer(StringVector(sizeof(str)*12÷10), true, true)
    out.size = 0
    out.ptr = 1
    ctr = 1
    minp = RepAgent("", "")
    while ctr <= count && n > 0
        
        minj::Int = typemax(Int)
        maxk::Int = -1
        for ii = 1:n
            pii = prp[ii]
            j, k = getnext(pii, maps, str, i)
            if j > 0 && (j < minj || j == minj && k > maxk)
                minj = j
                maxk = k
                minp = pii.id
            end
        end
        j = 0
        for ii = 1:n
            if prp[ii].r1 > 0
                j += 1
                if ii > j
                    prp[j] = prp[ii]
                end
            end
        end
        resize!(prp, j)
        if j < n
            println("reduced i= $i n=$j: $prp\n")
        end
        n = j
        
        j, k = (maxk == -1 ? 0 : minj), maxk

        j == 0 && break
        if i == sta || i <= k
            unsafe_write(out, pointer(str, i), UInt(j-i))
            _replace1(out, minp, str, j:k)
        end
        if k < j
            i = j
            j > eos && break
            k = nextind(str, j)
        else
            i = k = nextind(str, k)
        end
        ctr += 1
    end
    write(out, SubString(str, i))
    String(take!(out))
end

function _replace1(out::IO, p::Pair{<:TF,<:TS}, s, r)
    _replace(out, second(p), s, r, first(p))
end

# The following is dedicated to single pat-repl.
# replace Char with function that compares with this Char

consol_pattern(pat::Union{Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}) = occursin(pat)
consol_pattern(pat::Char) = equalto(pat)
consol_pattern(pat::AbstractString) = length(pat) == 1 ? equalto(pat[1]) : string(pat)
consol_pattern(pat::Regex) = pat
consol_pattern(pat::Callable) = pat
consol_pattern(pat) = string(pat) # TODO or argument error?

consol_repl(repl::Char) = string(repl)
consol_repl(repl::AbstractString) = string(repl)
consol_repl(repl::Base.SubstitutionString) = repl
consol_repl(repl::Callable) = repl
consol_repl(repl) = string(repl)

# replace collection of Char with function that checks occurrence in this collection
function predicatepair(p::Pair{<:TIF,<:TIS})::Pair{<:TF,<:TS}
    pat::TF  = consol_pattern(first(p))
    repl::TS = consol_repl(last(p))
    (repl isa Base.SubstitutionString) && !(pat isa Regex) && error("subs str req regex")
    pat === first(p) && repl === second(p) ? p : (pat=>repl)
end

