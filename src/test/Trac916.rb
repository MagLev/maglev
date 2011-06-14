
# Trac 916

class C
  VALID = [100..102]
  def test
    a = VALID
    case 101 when *a
      $ax = 916
    end
  end
end
C.new.test
unless $ax == 916 ; raise 'fail'; end
true
