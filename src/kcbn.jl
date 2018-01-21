
export replace_kcbn, write_funlist, ex_funlist
import Base: Callable, SubstitutionString

const TF = Union{String,Callable,Regex}
const TS = Union{Char,String,Callable,SubstitutionString}

const TIF = Union{Char,AbstractString,Regex,Callable,Tuple{Vararg{Char}},AbstractVector{Char},Set{Char}}
const TIS = Union{Char,AbstractString,Callable,SubstitutionString}

const TYPE_STRING  = 1
const TYPE_REGEX   = 2
const TYPE_FUNC    = 3

mutable struct PatternRepl
    pattern_type::Int   # Any of the constants TYPE_ above 
    repl_type::Int      # Any of the constants TYPE_ above
    r1::Int             # pre-fetched range1
    r2::Int             # pre-fetched range2
    pat_string::String  # only if pattern_type == TYPE_STRING
    pat_regex::Regex    #                              REGEX                     
    pat_func::Callable  #                              FUNC 
    rep_string::String  # only if repl_type == TYPE_STRING
    rep_subs::SubstitutionString #                 _REGEX 
    rep_func::Callable  #                          _FUNC 

    PatternRepl(pt::Int, rt::Int) = new(pt, rt, 0, 0)
end

function PatternRepl(pat::String, repl::String)
    pr = PatternRepl(TYPE_STRING, TYPE_STRING)
    pr.pat_string = pat
    pr.rep_string = repl
    pr
end

function PatternRepl(pat::String, repl::Callable)
    pr = PatternRepl(TYPE_STRING, TYPE_FUNC)
    pr.pat_string = pat
    pr.rep_func = repl
    pr
end

function PatternRepl(pat::Regex, repl::String)
    pr = PatternRepl(TYPE_REGEX, TYPE_STRING)
    pr.pat_regex = pat
    pr.rep_string = repl
    pr
end

function PatternRepl(pat::Regex, repl::SubstitutionString)
    pr = PatternRepl(TYPE_REGEX, TYPE_REGEX)
    pr.pat_regex = pat
    pr.rep_subs = repl
    pr
end

function PatternRepl(pat::Regex, repl::Callable)
    pr = PatternRepl(TYPE_REGEX, TYPE_FUNC)
    pr.pat_regex = pat
    pr.rep_func = repl
    pr
end

function PatternRepl(pat::Callable, repl::String)
    pr = PatternRepl(TYPE_FUNC, TYPE_STRING)
    pr.pat_func = pat
    pr.rep_string = repl
    pr
end

function PatternRepl(pat::Callable, repl::Callable)
    pr = PatternRepl(TYPE_FUNC, TYPE_FUNC)
    pr.pat_func = pat
    pr.rep_func = repl
    pr
end

# code generation for all patterns
function gen_pattern_all(prs::Vector{PatternRepl})
    Expr(:block, [gen_pattern(i, prs[i]) for i in 1:length(prs)]...)
end
# code generation depending on pattern_type
function gen_pattern(pri::Int, pr::PatternRepl)
    if pr.pattern_type == TYPE_STRING
        :(  let pr = prs[$pri], j = pr.r1, k;
            if j < typemax(Int)
                j, k = findnext_string(pr, str, pos)
                $(gen_min_max(pri))
            end
        end
        )
    elseif pr.pattern_type == TYPE_REGEX
        :(  let pr = prs[$pri], j = pr.r1, k;
            if j < typemax(Int)
               j, k = findnext_regex(pr, str, pos)
               $(gen_min_max(pri))
            end
        end
        )
    elseif pr.pattern_type == TYPE_FUNC
        :(  let j = $(getpr(1, pri)), k;
            if j < typemax(Int)
                if j < pos
                    j = findnext($(pr.pat_func), str, pos)
                    j = ifelse(j == nothing, typemax(Int), j)
                    $(setpr(1, pri, :j))
                    $(setpr(2, pri, :j))
                end 
                k = j
                $(gen_min_max(pri))
            end
        end
        ) 
    end
end
function gen_min_max(pri::Int)
    :( if j < minj || ( j == minj && k > maxk)
        minj, maxk = j, k
        minp = $pri
    end
   )
end

getpr(k::Int, pri::Int) = Symbol("pr", k, "_", pri)
setpr(k::Int, pri::Int, j) = Expr(:(=), getpr(k, pri), j)
setprall(k::Int, n::Int) = [setpr(k, i, 0) for i = 1:n] 

function findnext_string(pr::PatternRepl, str::String, cpos::Int)
    j = pr.r1
    if j < cpos
        r = findnext(pr.pat_string, str, cpos)
        j, k = first(r), last(r)
        j = ifelse(j == 0, typemax(Int), j)
        pr.r1, pr.r2 = j, k
    end
    j, k
end

function findnext_regex(pr::PatternRepl, str::String, cpos::Int)
    j = pr.r1
    if j < cpos
        r = findnext(pr.pat_regex, str, cpos)
        j, k = first(r), last(r)
        j = ifelse(j == 0, typemax(Int), j)
        pr.r1, pr.r2 = j, k
    end
    j, k
end

# cover the multiple pairs case
function replace_kcbn(str::String, pat_repls::Pair{<:TIF,<:TIS}...)
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
    prs::Vector{PatternRepl} = Vector{PatternRepl}(uninitialized, n)
    for ii = 1:n
        mapp[ii], mapr[ii] = predicatepair_kcan(pat_repls[ii])
        prs[ii] = PatternRepl(mapp[ii], mapr[ii])
    end
    gen_all = gen_pattern_all(prs)

    strf() = str

    ex = quote (rstr::Ref{String}, prs::Vector{PatternRepl}) ->
        let $(setprall(1, n)...), $(setprall(2, n)...)
        str = rstr[]
        eos = sizeof(str)
        pos::Int = 1
        out = IOBuffer(StringVector(eos), true, true)
        out.size = 0
        ctr = 1
        while ctr <= $count && pos <= eos
            
            minj::Int = typemax(Int)
            maxk::Int = -1
            minp::Int = 0
            $gen_all
            maxk == -1 && break
            if pos <= minj
                unsafe_write(out, pointer(str, pos), UInt(minj-pos))
                $(ex_funlist(mapp, mapr))
            end
            pos = nextind(str, max(minj, maxk))
            ctr += 1
        end
        write(out, SubString(str, pos))
        String(take!(out))
        end
    end
    fun = eval(ex)
    Base.invokelatest(fun, Ref(str), prs)
end
 
function ex_funlist(mapp::Vector{TF}, mapr::Vector{TS})
    n = length(mapp)
    list = Vector{Tuple}(); sizehint!(list, n)
    CT = (Char,)
    for i = 1:n
        repl = mapr[i]
        pat = mapp[i]
        if repl isa String
            push!(list, (i, 2, repl))
        elseif repl isa Function # is there a method repl(::Char) ? 
            usechar = pat isa Function ? method_exists(repl, CT) : false
            push!(list, (i, usechar, repl))
        elseif repl isa SubstitutionString
            push!(list, (i, 3, repl, pat))
        end
    end

    function ex_single(bop::Tuple)
        i, cf, op = bop
        if cf == 0
            :(write(out, $op(getindex(str, minj:maxk))))
        elseif cf == 1
            :(write(out, $op(getindex(str, minj))))
        elseif cf == 2
            :(write(out, prs[$i].rep_string)) 
        elseif cf == 3
            :(_replace(out, prs[$i].rep_subs, str, minj:maxk, prs[$i].pat_regex))
        end
    end
    
    ex_if_elseif_end(ex_single, list)
end

function ex_if_elseif_end(ex_single::Function, list)

    ex_if(nr, op) = Expr(:if, :(minp == $nr), ex_single(op))
    ex_elif(nr, op) = Expr(:elseif, :(minp == $nr), ex_single(op))
    ex_append!(ex1::Expr, ex2::Expr) = push!(ex1.args, ex2)
   
    n = length(list)
    if n == 0
        :nothing
    elseif n == 1
        ex_single(list[1])
    else
        ex::Expr = ex_if(list[1][1], list[1])
        ex1 = ex
        for i = 2:n
            ex2::Expr = ex_elif(list[i][1], list[i])
            ex_append!(ex1, ex2)
            ex1 = ex2
        end
        ex
    end
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

