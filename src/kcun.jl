
export replace_kcun
import Base.Callable

const TF = Union{String,Callable,Regex}
const TS = Union{String,Callable,Base.SubstitutionString}

const TIF = Union{Char,AbstractString,Regex,Callable,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}
const TIS = Union{Char,AbstractString,Callable,Base.SubstitutionString}

mutable struct RepAgent{F<:TF,S<:TS}
    pattern::F
    repl::S
    r1::Int
    r2::Int
end
RepAgent(pat::F, repl::S) where {F<:TF,S<:TS} = RepAgent{F,S}(pat, repl, 0, 0)

# cover the multiple pairs case
function replace_kcun(str::String, pat_repls::Pair{<:TIF,<:TIS}...)
    count::Integer=typemax(Int)

    if count == 0 || length(pat_repls) == 0
        return str
    end
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))

    n = length(pat_repls)
    # the following 4 vectors are kept parallel (corresponding elements)
    #=
    prp::Vector{RepAgent{TF,TS}} = Vector{RepAgent{TF,TS}}(uninitialized, n)
    for ii = 1:n
        prp[ii] = predicatepair(pat_repls[ii]) # pattern-repl-pair being processed
    end
    =#
    prp::Vector{RepAgent{<:TF,<:TS}} = predicatepair.(collect(pat_repls))
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
            if pii.r1 < i
                _findnext!(pii, str, i)
            end
            j, k = pii.r1, pii.r2
            if j > 0 && (j < minj || j == minj && k > maxk)
                minj = j
                maxk = k
                minp = pii
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

function _replace1(out::IO, ra::RepAgent{<:TF,<:TS}, s, r)
    _replace(out, ra.repl, s, r, ra.pattern)
end

function _findnextxxx!(p::RepAgent{<:TF,<:TS}, str, sta)
    _findnext!(p, str, sta)
end

function _findnext!(p::RepAgent{<:Callable,<:TS}, str::String, sta::Int)
    j = findnext(p.pattern, str, sta)
    p.r1 = p.r2 = j
end

function _findnext!(p::RepAgent{String,<:TS}, str::String, sta::Int)
    r::UnitRange{Int} = findnext(p.pattern, str, sta)
    p.r1, p.r2 = first(r), last(r)
end

function _findnext!(p::RepAgent{Regex,<:TS}, str::String, sta::Int) 
    r::UnitRange{Int} = findnext(p.pattern, str, sta)
    p.r1, p.r2 = first(r), last(r)
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
function predicatepair(p::Pair{<:TIF,<:TIS})::RepAgent{<:TF,<:TS}
    pat = consol_pattern(first(p))
    repl = consol_repl(last(p))
    RepAgent(pat, repl)
end

