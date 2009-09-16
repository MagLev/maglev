string = ""
book = "So Long, and Thanks for All the Fish"
[10_000_000].map do |n|
    n.times {|i| string << book }
end
