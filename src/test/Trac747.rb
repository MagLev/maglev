# From Rack::Mount::Multimap
#
# MagLev is wrapping "path.info" in an array.

class Multimap
  def store(*args)
    p args
    raise "Fail: args[0] should be a string" unless args[0].class == String
  end
  alias_method :[]=, :store
end

a = "path.info"
m = Multimap.new
m[*a] = :foo
