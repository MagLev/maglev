# Trac 728

class SubArr < Array
  def gethist
    @hist
  end 
end
class SubArr
  def initialize(arg)
    @name = arg
    @hist = "#{arg}_hist"
  end 

  def getnam
    @name
  end
end

x = SubArr.new("first")
x << "one"
x << "one"
x << "two"

y = x.uniq
unless y == ["one", "two"] ; raise 'fail A'; end
unless y.getnam == 'first' ; raise 'fail B'; end
unless y.gethist == 'first_hist' ; raise 'fail B'; end
true
