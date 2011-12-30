# https://github.com/MagLev/maglev/issues/116
#

require File.expand_path('simple', File.dirname(__FILE__))

class Foo
end

foo = Foo.new
begin
  foo.bar
rescue NoMethodError => e
  test e.message, "NoMethodError: undefined method `bar' for Foo", "Undefined method for instance of Foo"
end

begin
  Foo.bar
rescue NoMethodError => e
  test e.message, "NoMethodError: undefined method `bar' for Foo:Class", "Undefined method for class object Foo"
end
