# Distilled from src/lib/rational.rb
class Bignum

  alias power! **
end

v = 1 << 64 
unless v.class.name == 'Bignum' ; raise 'error'; end
v = v.power!(3)
e = 1 << 192 
unless v == e ; raise 'error'; end
true
