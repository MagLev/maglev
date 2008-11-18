# Tests for methods in Kernel.rb
require File.expand_path('simple', File.dirname(__FILE__))

class Foo
  def method_missing(method_id, *args)
    if method_id == :foo
      args.length
    else
      super
    end
  end
end

f = Foo.new

test(f.foo,           0,  "f.foo")
test(f.foo('a'),      1,  "f.foo")
test(f.foo('a', 'b'), 2,  "f.foo")

begin
  f.bar('a', 'b')
  # Fail! should have an exception
  failed_test('f.bar', 'Error', 'NoError')
rescue
  # OK!
end

report
true
