$a = [ 0 ]
$b = 0
X = 4
Y = -1 
module MA
  X = 5
  Y = X 
  module MA
   X = Y + 1
   module MA
     X = 7
     $b = MA::X + 1000
     $a[3] = X # gets 7
     $a[0] = ::X # gets 4
     # puts MA::MA::X # gets error
   end
   module MA
     X = 8 # already initialized warning
   end
  end
end
class MyError < Exception
  EA = 105
end
# puts "expect 4 5 6 7 8 105 "
r = $a[0]
unless r == 4
  raise 'ERROR'
end
r = MA::X  # gets 5
unless r == 5 
  raise 'ERROR'
end
r = MA::MA::X # gets 6
unless r == 6
  raise 'ERROR'
end
r = $a[3]
unless r == 7
  raise 'ERROR'
end
r = MA::MA::MA::X # gets 8
unless r == 8
  raise 'ERROR'
end
r = MyError::EA
unless r == 105
  raise 'ERROR'
end
r = $b
unless r == 1007
  raise 'ERROR'
end
true
