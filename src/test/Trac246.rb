# ##### Trac # 246 #######################################################
# From mspec.rb
require File.expand_path('simple', File.dirname(__FILE__))

class Foo
  def self.store(symbol, value)
    # This line blows up:
    instance_variable_set :"@#{symbol}", value
  end

  def self.retrieve(symbol)
    instance_variable_get :"@#{symbol}"
  end
end

val = "x"
ff = Foo
Foo.store(:foo, val)

test(Foo.retrieve(:foo), val, 'the test')

report
