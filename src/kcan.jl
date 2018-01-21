
export replace_kcan, Agent
import Base.Callable

const TF = Union{String,Callable,Regex}
const TS = Union{Char,String,Callable,Base.SubstitutionString}

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

function getnext(ra::Agent, mapp::Vector{TF}, s::String, ii::Int)
    if ra.j == 0 || ra.j >= ii
        return (ra.j, ra.k)
    end
    pos = ra.pos + 1
    n = length(ra.r1)
    # dicard all overlapping entries
    while pos <= n && 0 < ra.r1[pos] < ii
        pos += 1
    end
    if pos > n
        _findnext!(mapp[ra.id], ra, s, ii)
        # point of type unstability - for big n negligible
        ra.pos = pos = 1
    end
    ra.pos = pos
    ra.j, ra.k = ra.r1[pos], ra.r2[pos]
    ra.j, ra.k
end

# The following functions should be type-stable
function _findnext!(p::String, ra::Agent, s::String, ii::Int) 
    n = length(ra.r1)
    i = ii
    for pos = 1:n
        j, k = findnext(p, s, i)
        ra.r1[pos], ra.r2[pos] = j, k
        j <= 0 && break
        k = i == ii ? k : i
        i = nextind(s, k)
    end
end

function _findnext!(p::Regex, ra::Agent, s::String, ii::Int) 
    n = length(ra.r1)
    i = ii
    for pos = 1:n
        j, k = findnext(p, s, i)
        ra.r1[pos], ra.r2[pos] = j, k
        j <= 0 && break
        k = i == ii ? k : i
        i = nextind(s, k)
    end
end

function _findnext!(p::Callable, ra::Agent, s::String, ii::Int)
    n = length(ra.r1)
    i = ii
    for pos = 1:n
        j = findnext(p, s, i)
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
    # maps::Vector{Pair{<:TF,<:TS}} = predicatepair_kcan.(collect(pat_repls))
    mapp = Vector{TF}(uninitialized, n)
    mapr = Vector{TS}(uninitialized, n)

    for ii = 1:n
        mapp[ii], mapr[ii] = predicatepair_kcan(pat_repls[ii])
    end
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
    minp = 0
    while ctr <= count && n > 0
        
        minj::Int = typemax(Int)
        maxk::Int = -1
        for ii = 1:n
            ra = prp[ii]
            j, k = getnext(ra, mapp, str, i)
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
            _replace1(out, mapp[minp], mapr[minp], str, j, k)
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

function _replace1(out::IO, ::TF, repl::String, s, j, k)
    print(out, repl)
end
function _replace1(out, ::TF, repl::Function, s, j, k)
    print(out, repl(SubString(s, j, k)))
end
function _replace1(out::IO, pat::TF, repl::Base.SubstitutionString, s, j, k)
    _replace(out, repl, s, j:k, pat)
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
    repl::TS = consol_repl(p.second)
    (repl isa Base.SubstitutionString) && !(pat isa Regex) && error("subs str req regex")
    pat === p.first && repl === p.second ? p : (pat=>repl)
end

