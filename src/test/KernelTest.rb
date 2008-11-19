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

test(String("foo"), "foo", 'String("foo")')
test(String(1),       "1", 'String(1)')

test(Float(1),       1.0, 'Float("1")')

test(Array(1),           [1], 'Array(1)')
test(Array([1,2,3]), [1,2,3], 'Array([1,2,3])')
test(Array(nil),          [], 'Array(nil)')

report
true
