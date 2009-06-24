require File.expand_path('simple', File.dirname(__FILE__))

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

test($a[0],           4, 'A')
test(MA::X,           5, 'B')
test($a[3],           7, 'C')
test(MA::MA::MA::X,   8, 'D')
test(MyError::EA,   105, 'E')
test($b,           1007, 'F')
test(MA::MA::X,       6, 'G')

report
