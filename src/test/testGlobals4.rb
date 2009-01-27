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

# ----- coverage for ticket 324
$AA = 88
$AB = 880
unless $AB == 880 ; raise 'Error' ; end

class C324
  def do_alias
    alias $AB $AA
  end
end

unless $AB == 880 ; raise 'Error' ; end
C324.new.do_alias

unless $AB == 88 ; raise 'Error' ; end
# ------- end ticket 324
