module A
  class B
    Number = 47
  end
end

class D < A::B
  def number
    Number  
  end
end

n = D.new.number
unless n == 47
  raise 'ERR'
end

e = 0
begin
  x =  :File::TEST 
rescue TypeError
  e = 22
end
unless e == 22
  raise 'ERR'
end

V = 3
begin
  V::W = 5
rescue TypeError
  e = 33
end
unless e == 33
  raise 'ERR'
end

begin
  class nil::Foo
  end
rescue TypeError
  e = 44
end
unless e == 44
  raise 'ERR'
end
 
class TFile
  ALT_X = 5
  module Constants
    ALT_SEPARATOR  = 6
    Separator      = ALT_SEPARATOR  # Problem1: This blows up
  end
  include Constants
end

#  maybe MAGLEV_SEEN MAGLEV_WARNSTUB  also
exp_constants = %w( ALT_SEPARATOR ALT_X Constants Separator )
cls = TFile
clist = TFile.constants.sort 
unless clist == exp_constants ; raise 'Err' ; end

true

