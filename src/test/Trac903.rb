
class Trac903
  def test
    [10,20,30].__send__ :last, *[]
  end
end

x = Trac903.new.test
unless x == 30 ; raise 'fail' ; end
true
