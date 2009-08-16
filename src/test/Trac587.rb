# coverage for Trac 587
#  constant lookup coherency.

# puts "--X5"
X = 55
# puts "--C"
class C
  # puts "--D"
  class D
    # puts "--X6"
    X = 66
    def ma
      X
    end
  end
end
o = C::D.new
r = o.ma
unless r == 66 ; raise 'error'; end
# puts "--remove"
C::D.remove_const( :X )
r = o.ma
unless r == 55 ; raise 'error'; end
class C
  # puts "--X7"
  X = 77
end
r = o.ma
unless r == 77 ; raise 'error'; end
true
