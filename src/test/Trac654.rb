class C
  @@c_var = 12
end
c = C.new

# MRI complains on this, but MagLev prints 12
class << c
  p @@c_var
end
