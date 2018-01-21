
export replace_kcbn, write_funlist, ex_funlist
import Base: Callable, SubstitutionString

const TF = Union{String,Callable,Regex}
const TS = Union{String,Callable,SubstitutionString}

const TIF = Union{Char,AbstractString,Regex,Callable,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}
const TIS = Union{Char,AbstractString,Callable,SubstitutionString}

# code generation for all patterns
function gen_pattern_all(ptypes::NTuple{N,DataType}) where N
    Expr(:block, [gen_pattern(i, ptypes[i]) for i in 1:N]...)
end

# code generation depending on pattern_type
function gen_pattern(pri::Int, ::Type{<:Pair{<:Union{Regex,String}}})
    :(  let j = $(getpr(1, pri)), k = $(getpr(2, pri)), r;
        if j < MAX
            if j < pos
                r = findnext($(getpa(pri)), str, pos)
                j, k = first(r), last(r)
                j = ifelse(j == 0, MAX, j)
                $(setpr(1, pri, :j)); $(setpr(2, pri, :k))
            end
            $(gen_min_max(pri))
        end
    end
    )
end

function gen_pattern(pri::Int, ::Type{<:Pair{<:Function}})
    :(  let j = $(getpr(1, pri)), k;
        if j < MAX
            if j < pos
                j = coalesce(findnext($(getpa(pri)), str, pos), MAX)
                $(setpr(1, pri, :j)); $(setpr(2, pri, :j))
            end 
            k = j
            $(gen_min_max(pri))
        end
    end
    ) 
end

function gen_min_max(pri::Int)
    :( (j < minj || j == minj && k > maxk) && ((minj, maxk, minp) = (j, k, $pri)) )
end

# generate code to select one of several options, depending on variable `minp`.
function extree(n::Int, m::Int, ex::Vector)
    if n < m
        k = (n + m) ÷ 2
        Expr(:if, :(minp <= $k), extree(n, k, ex), extree(k+1, m, ex))
    elseif n == m
        ex[n]
    else
        nothing
    end
end

getpr(k::Int, pri::Int) = Symbol("pr", k, "_", pri)
setpr(k::Int, pri::Int, j) = Expr(:(=), getpr(k, pri), j)
setprall(k::Int, n::Int) = [setpr(k, i, 0) for i = 1:n] 

getpa(pri::Int) = Symbol("pa_", pri)
setpa(pri::Int, T::Type, j) = Expr(:(=), Expr(:(::), getpa(pri), T), j)
setpaall(pt::NTuple{N,DataType}) where N = [setpa(i, pt[i].types[1], :(pat_repls[$i].first)) for i = 1:N]
getre(pri::Int) = Symbol("re_", pri)
setre(pri::Int, T::Type, j) = Expr(:(=), Expr(:(::), getre(pri), T), j)
setreall(pt::NTuple{N,DataType}) where N = [setre(i, pt[i].types[2], :(pat_repls[$i].second)) for i = 1:N]


# cover the multiple pairs case
function replace_kccn(str::String, pat_repls::Pair{<:TIF,<:TIS}...)
    count::Integer=typemax(Int)

    if count == 0 || length(pat_repls) == 0
        return str
    end
    count < 0 && throw(DomainError(count, "`count` must be non-negative."))

    replace_gen(str, count, pat_repls...)
end

function gen_repl_all(ptypes::NTuple{N,DataType}) where N
    extree(1, N, [gen_repl(i, ptypes[i]) for i in 1:N])
end

# code generation depending on pattern_type
function gen_repl(pri::Int, ::Type{<:Pair{<:Any, String}})
    :( write(out, $(getre(pri))))
end

function gen_repl(pri::Int, ::Type{<:Pair{<:Any, <:Callable}})
    :( write(out, $(getre(pri))(getindex(str, minj:maxk))))
end

function gen_repl(pri::Int, ::Type{<:Pair{Regex, <:SubstitutionString}})
    :(_replace(out, $(getre(pri)), str, minj:maxk, $(getpa(pri))))
end

function replace_gen_impl(str::Type{String}, count::Type{Int}, pat_repls...)
    n = length(pat_repls)
    gen_input = gen_pattern_all(pat_repls)
    gen_output = gen_repl_all(pat_repls)

    quote
        begin
            $(setpaall(pat_repls)...)
            $(setreall(pat_repls)...)
            $(setprall(1, n)...)
            $(setprall(2, n)...)
            n::Int = $n
            MAX = typemax(Int)
            eos = sizeof(str)
            pos::Int = 1
            out = IOBuffer(Base.StringVector(eos), true, true)
            out.size = 0
            ctr = 1
            while ctr <= count && pos <= eos
                
                minj::Int = MAX
                maxk::Int = -1
                minp::Int = 0
                $gen_input
                maxk == -1 && break
                if pos <= minj
                    unsafe_write(out, pointer(str, pos), UInt(minj-pos))
                    $gen_output
                end
                pos = nextind(str, max(minj, maxk))
                ctr += 1
            end
            write(out, SubString(str, pos))
            String(take!(out))
        end
    end
end

@generated function replace_gen(str::String, count::Int, pat_repls::Pair...)
    replace_gen_impl(str, count, pat_repls...)
end

_replace(io, repl, str, r, pattern) = print(io, repl)
_replace(io, repl::Function, str, r, pattern) =
    print(io, repl(SubString(str, first(r), last(r))))

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
function predicatepair_kccn(p::Pair{<:TIF,<:TIS})::Pair{<:TF,<:TS}
    pat::TF  = consol_pattern(first(p))
    repl::TS = consol_repl(p.second)
    repl isa Base.SubstitutionString && !(pat isa Regex) && error("substitution string requires regex")
    pat === p.first && repl === p.second ? p : (pat=>repl)
end
