
export hard, soft, soft_intern, soft_unrolled_intern, soft_unrolled
export exchange!, substitute!, substitute

# hard coded function calls in inner loop
function hard(s::String)
    io = IOBuffer()
    for c in s
        c = lowercase(c)
        c = c + 20
        c = uppercase(c)
        c = c - 20
        write(io, c)
    end
    String(take!(io))
end

# the same functionality, more flexible with a list of
# functions as arguments
# performance is 
function soft_intern(s::String, funlist::Function...)
    io = IOBuffer()
    for c in s
        for fun in funlist
            c = fun(c)
        end
        write(io, c)
    end
    String(take!(io))
end

# make use of the flexible implementation
function soft(s::String)
    soft_intern(s, lowercase, c->c+20, uppercase, c->c-20)
end

# an attempt to improve perfomance
# works!
function soft_unrolled_intern(funlist::Function...)

    function code_for_funcall(fun::Function)
        :( c = $(fun)(c) )
    end
    function unrolled()
        Expr(:block, code_for_funcall.(funlist)...)
    end

    ex = :(
       (s::String) ->
       let c::Char, io = IOBuffer()
           for c in s
               $(unrolled())
               write(io, c)
           end
           String(take!(io))
       end
    )
    f = eval(ex)
    (x) -> Base.invokelatest(f, x)
end

"""

    soft_unrolled(s::String)

The following data show the timing, which is achieved by the different implementations.
When the function is generated, and executed separately, perfomance is
as good as the hard-coded version!

```jldoctest
julia> srand(0); s = randstring(10^7);

julia> @time hard(s);
  0.497349 seconds (13.58 k allocations: 16.785 MiB)

julia> @time hard(s);
  0.469577 seconds (28 allocations: 16.003 MiB)

julia> @time soft(s);
  2.087998 seconds (9.70 M allocations: 165.476 MiB, 2.98% gc time)

julia> @time soft(s);
  2.015281 seconds (9.67 M allocations: 171.554 MiB, 2.53% gc time)

julia> @time soft_unrolled(s);
  0.568373 seconds (38.56 k allocations: 18.327 MiB)

julia> @time soft_unrolled(s);
  0.506165 seconds (19.58 k allocations: 17.188 MiB, 4.41% gc time)

julia> @time ex = soft_unrolled_intern(lowercase, c->c+20, uppercase, c->c-20)
  0.020691 seconds (6.41 k allocations: 413.712 KiB)
#5 (generic function with 1 method)

julia> @time ex(s);
  0.565612 seconds (19.58 k allocations: 17.129 MiB, 9.19% gc time)

julia> @time ex(s);
  0.477627 seconds (37 allocations: 16.003 MiB)

julia> @time ex(s);
  0.479458 seconds (37 allocations: 16.003 MiB)
"""
function soft_unrolled(s::String)
    soft_unrolled_intern(lowercase, c->c+20, uppercase, c->c-20)(s)
end

# make that generic
function substitute!(outer::Expr, critical, list)
    function code_for_funcall(element)
        exchange!(deepcopy(critical), :( :REPLACE ), :( $element ))
    end

    function unrolled()
        Expr(:block, code_for_funcall.(list)...)
    end

    exchange(outer, :( :BLOCK ), unrolled())
end

substitute(outer::Expr, critical, list) = substitute!(deepcopy(outer), critical, list)

function exchange!(code::Expr, dummy, replacement) 
    if dummy == code
        replacement
    else
        for i = 1:length(code.args)
            code.args[i] = exchange(code.args[i], dummy, replacement)
        end
    end
    code
end

function exchange!(code::Any, dummy::Any, replacement::Any)
    code == dummy ? replacement : code
end

