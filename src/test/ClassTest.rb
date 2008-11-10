require File.expand_path('simple', File.dirname(__FILE__))

# Trac ticket #206 fix
class Foo
  @bar = false
  class << self
    attr_accessor :bar
  end
end

test(Foo.bar, false, 'A')

report
true
