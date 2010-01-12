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
hh[:one] = 1
hh[:two] = 2

hh.each do |k,v|
  puts "#{k} ===> #{v}"
end
