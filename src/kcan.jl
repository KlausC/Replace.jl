
export replace_kcan, Agent
import Base.Callable

const TF = Union{String,Callable,Regex}
const TS = Union{String,Callable,Base.SubstitutionString}

const TIF = Union{Char,AbstractString,Regex,Callable,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}
const TIS = Union{Char,AbstractString,Callable,Base.SubstitutionString}

mutable struct Agent
    id::Int             # Index into original list of pattern-replacement pairs 
    pos::Int            # previously read position or 0
    j::Int              # current range.1
    k::Int              # current range.2
    r1::Vector{Int}     # pre-stored range.1    
    r2::Vector{Int}     # pre-stored range.2

    Agent(id::Int, bufl::Int) = new(id, bufl+1, -1, 0, fill(0, bufl), fill(0, bufl))
end

function Base.isempty(ra::Agent)
    ra.j == 0 
end

function getnext(ra::Agent, maps::Vector{<:Pair{<:TF,<:TS}},s::String, ii::Int)
    # dicard all overlapping entries
    n = length(ra.r1)
    ra.j == 0 && return (0, -1)
    ra.j >= ii && return (ra.j, ra.k)
    pos = ra.pos + 1
    while pos <= n && 0 < ra.r1[pos] < ii
        pos += 1
    end
    if pos > n
        _findnext!(maps[ra.id], ra, s, ii)
        # point of type unstability - for big n negligible
        ra.pos = pos = 1
    end
    ra.pos = pos
    ra.j, ra.k = ra.r1[pos], ra.r2[pos]
    ra.j, ra.k
end

# The following functions should be type-stable
function _findnext!(p::Pair{String}, ra::Agent, s::String, ii::Int) 
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

function _findnext!(p::Pair{Regex}, ra::Agent, s::String, ii::Int) 
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

function _findnext!(p::Pair{<:Callable}, ra::Agent, s::String, ii::Int)
    n = length(ra.r1)
    i = ii
    for pos = 1:n
        j = findnext(first(p), s, i)
        ra.r1[pos], ra.r2[pos] = j, j
        j <= 0 && break
        i = nextind(s, j)
    end
end

# cover the multiple pairs case
function replace_kcan(str::String, pat_repls::Pair{<:TIF,<:TIS}...)
    count::Integer=typemax(Int)

    if count == 0 || length(pat_repls) == 0
        return str
    end
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))
    bufl = 100

    n = length(pat_repls)
    # the following 4 vectors are kept parallel (corresponding elements)
    maps::Vector{Pair{<:TF,<:TS}} = predicatepair_kcan.(collect(pat_repls))
    prp::Vector{Agent} = Agent.(1:n, bufl)
    eos = endof(str)
    i::Int = 1
    sta::Int = i
    k::Int = i
    j::Int = 0
    out = IOBuffer(StringVector(sizeof(str)*12÷10), true, true)
    out.size = 0
    out.ptr = 1
    ctr = 1
    minp = Pair("", "")
    while ctr <= count && n > 0
        
        minj::Int = typemax(Int)
        maxk::Int = -1
        for ii = 1:n
            ra = prp[ii]
            j, k = getnext(ra, maps, str, i)
            if j > 0 && (j < minj || j == minj && k > maxk)
                minj = j
                maxk = k
                minp = ra.id
            end
        end
        j = 0
        for ii = 1:n
            if !isempty(prp[ii])
                j += 1
                if ii > j
                    prp[j] = prp[ii]
                end
            end
        end
        resize!(prp, j)
        n = j
        
        j, k = (maxk == -1 ? 0 : minj), maxk

        j == 0 && break
        if i == sta || i <= k
            unsafe_write(out, pointer(str, i), UInt(j-i))
            _replace1(out, maps[minp], str, j:k)
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
    _replace(out, last(p), s, r, first(p))
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
function predicatepair_kcan(p::Pair{<:TIF,<:TIS})::Pair{<:TF,<:TS}
    pat::TF  = consol_pattern(first(p))
    repl::TS = consol_repl(last(p))
    (repl isa Base.SubstitutionString) && !(pat isa Regex) && error("subs str req regex")
    pat === first(p) && repl === last(p) ? p : (pat=>repl)
end

