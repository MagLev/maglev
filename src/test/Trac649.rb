# The following causes a stack overflow in MagLev.
# Inspired from Rack 1.1.0.

class HeaderHash < Hash
  def each
    super do |k,v|
      yield(k, v)
    end
  end
end

hh = HeaderHash.new
hh['one'] = 11
hh['two'] = 22

arr = []
hh.each do |k,v|
  arr << k
  arr << v
end

unless arr = [ 'one', 11 , 'two', 22 ] ; raise 'error' ; end

true
