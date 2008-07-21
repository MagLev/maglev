
arr = Array.new(5)
arr[0] = "zero"
arr[1] = "one"
arr[2] = "two"
arr[3] = "three"

puts "array size = " + arr.size.to_s
for i in 0 ... arr.size
  puts 'arr[' + i.to_s + '] = ' + arr[i].to_s
end
