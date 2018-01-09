

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
