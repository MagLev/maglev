# top_self is the top level object for MagLev.  It is in session temps
# #'RubyMainSelf'.  It has special singleton methods installed on it.  The
# MRI impl is in ruby eval.c top_include(), top_private() and top_public().

require File.expand_path('simple', File.dirname(__FILE__))

test(to_s, "main", "top_self.to_s")
test(inspect, "main", "top_self.inspect")

module Foo
  def foo
    :Foo_foo
  end
end

include Foo

test(foo, :Foo_foo, "Foo#foo")

# Right now, just test that these can be called.
private(:foo)
public(:foo)

report

