$b = 0
module MA
  def foo()
    $b = MA::X 
  end
  module Tms
    X=99
  end
  X = MA::Tms::X
end
r = $b
unless r == 0
  raise 'ERROR'
end
r = MA::X
unless r == 99
  raise 'ERROR'
end
true
