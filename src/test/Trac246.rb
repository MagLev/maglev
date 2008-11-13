# ##### Trac # 246 #######################################################
# From mspec.rb
class Foo
  def self.store(symbol, value)
    # This line blows up:
    instance_variable_set :"@#{symbol}", value
  end

  def self.retrieve(symbol)
    instance_variable_get :"@#{symbol}"
  end
end

Foo.store(:foo, "x")
puts "value of :foo #{Foo.retrieve :foo}"
