module MA 
  W=9  
  module MB
    MB::X=55
    d = X
    X=555  # warning already init
    W=8
    module MC
       XC=99
    end
  end
  H = {}
  H['abc'] = "myStringAbc"
end
class MyError < Exception 
  EA = 105 
  EB = MA::MB::X 
  # 'ab3'.pause
end
Z=77
# a = MA::Y gives error
b = MA::MB::X
MA::MB::X = b  # warning already init
c = ::Z
d = MA::MB::MC::XC
e = MA::H['abc']
mea = MyError::EA
meb = MyError::EB
# 'ab4'.pause
unless b ==  555
  raise 'ERROR'
end
unless c == 77
  puts c
  raise 'ERROR'
end
unless d == 99
  raise 'ERROR'
end
unless e == 'myStringAbc'
  raise 'ERROR'
end
unless mea == 105
  raise 'ERROR'
end
unless meb == 555
  raise 'ERROR'
end
r = MA::W  
unless r == 9
  raise 'ERROR'
end
r = MA::MB::W 
unless r == 8
  raise 'ERROR'
end
true

