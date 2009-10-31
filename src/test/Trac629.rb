# Should never print 'false'
 
# L: ary, S: nil, R: argscat
puts "a, b, c = 1, *2"
a, b, c = 1, *2
p a == 1
p b == 2
p c == nil
 
puts "a, b, c = 1, 2, *3"
a, b, c = 1, 2, *3
p a == 1
p b == 2
p c == 3
 
# L: ary, S: x, R: argscat
puts "a, *b = 1, *2"
a, *b = 1, *2
p a == 1
p b == [2]
 
puts "a, *b = 1, 2, *3"
a, *b = 1, 2, *3
p a == 1
p b == [2, 3]
 
# L: nil, S: x, R: argscat
puts "*a = 1, *2"
*a = 1, *2
p a == [1, 2]
