# From Rack::Mount::Multimap
#
# MagLev is wrapping "path.info" in an array.

class Multimap
  def store(*args)
    p args
    raise "Fail: args[0] should be a string" unless args[0].class == String
  end
  alias_method :[]=, :store

  def self.test
    a = ["path.info"]
    m = Multimap.new
    m[*a] = :foo
  end
  def self.test2
    a = 'bxx'
    m = Multimap.new
    m[*a] = :foo
  end
end

Multimap.test
Multimap.test2

