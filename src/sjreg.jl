
export replace_sjreg, replace_sjhand, replace_sjhand2

function replace_sjreg(s::AbstractString, p::Pair{<:AbstractString,<:AbstractString}...)
    d = Dict(p...)
    buf = IOBuffer()
    frst = true
    for pair in p
        frst || print(buf, '|')
        frst = false
        escape_string(buf, first(pair), "[\\^\$.|?*+()")
    end
    r = Regex(String(take!(buf)))
    replace(s, r => (s -> d[s]))
end


function replace_sjhand(s::AbstractString)
    io = IOBuffer()
    for c in s
        if c == 'x'
            c = 'A'
        elseif c == 'b'
            c = 'B'
        elseif islower(c)
            c = uppercase(c)
        end
        write(io, c)
    end
    String(take!(io))
end

function replace_sjhand2(s::AbstractString)
    buf = IOBuffer()
    for c in s
        if c == 'c'
            print(buf, "BOOM")
        elseif !isupper(c)
            print(buf, c)
        end
    end
    String(take!(buf))
end
