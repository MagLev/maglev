# Distilled from src/lib/rational.rb
class Bignum

  alias power! **
end

v = 1 << 64 
vcl = v.class
vn = vcl.name
unless vn == 'Bignum' ; raise 'error'; end
vp = v.power!(3)
e = 1 << 192 
unless vp == e ; raise 'error'; end
true
