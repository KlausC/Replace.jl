
export replace_sjreg, replace_sjhand

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
        if c == 'a'
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

