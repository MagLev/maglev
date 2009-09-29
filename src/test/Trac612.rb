n = 5
Open = "open"
Closed = "closed"
def Open.f
    Closed
end
def Closed.f
    Open
end
doors = [Closed] * (n+1)
for mul in 1..n
#   puts "mul = #{mul.inspect}"
    for x in 1..n
#       puts "x = #{x.inspect}"
# nil.pause
        doors[mul*x] = (doors[mul*x] || break).f
    end
end
doors.each_with_index {
    |b, i|
    puts "Door #{i} is #{b}" if i>0
}
 
