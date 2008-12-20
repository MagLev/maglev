require File.expand_path('simple', File.dirname(__FILE__))

# Pickaxe tests

module Other
  def three()
  end
end

class Single
  def Single.four()
  end
end

a = Single.new
def a.one()
end

class << a
  include Other
  def two()
  end
end

test(Single.singleton_methods, ["four"], 'Single.singleton_methods')

test(a.singleton_methods(false).sort,
     ["one", "two"], 'a.singleton_methods(false)')

test(a.singleton_methods(true).sort,
     ["one", "three", "two"], 'a.singleton_methods(true)')

test(a.singleton_methods.sort,
     ["one", "three", "two"], 'a.singleton_methods')


# Test if a method appears twice if it is in the singleton class and an
# extended module

o = Object.new
def o.foo;  1; end
module Foo
  def foo
    0
  end
  def bar
    12
  end
end
o.extend Foo

test(o.singleton_methods(true).sort,  ["bar", "foo"], "o true")
test(o.singleton_methods(false).sort, ["foo"],        "o false")

# Test if a method appears twice if it is in the singleton class and an
# included module

class C
  include Foo
  def self.foo; 1; end
end
class D
  def self.foo; 1; end
  include Foo
end

test(C.singleton_methods(true).sort,  ["foo"], "include C true")
test(C.singleton_methods(false).sort, ["foo"], "include C false")
test(C.foo, 1, "include C.foo")

test(D.singleton_methods(true).sort,  ["foo"], "include D true")
test(D.singleton_methods(false).sort, ["foo"], "include D false")
test(D.foo, 1, "include D.foo")

class E
  extend Foo
  def self.foo; 1; end
end
class F
  def self.foo; 1; end
  extend Foo
end

test(E.singleton_methods(true).sort,  ["bar", "foo"], "extend E true")
test(E.singleton_methods(false).sort, ["foo"],        "extend E false")
test(E.foo, 1, "extend E.foo")

test(F.singleton_methods(true).sort,  ["bar", "foo"], "extend F true")
test(F.singleton_methods(false).sort, ["foo"],        "extend F false")
test(F.foo, 1, "extend F.foo")

# Test on an object with out a singleton class

o = Object.new
test(o.singleton_methods(true),  [], "no singleton true")
test(o.singleton_methods(false), [], "no singleton false")

o.extend Foo
test(o.singleton_methods(true),  ["foo", "bar"], "o extended true")
test(o.singleton_methods(false), [],             "o extended false")


class PP
  module ObjectMixin
    def method_from_object_mixin
    end
  end
end

class Object
  include PP::ObjectMixin
end


test(Object.singleton_methods(true), [],  'ObjectMixin class true')
test(Object.singleton_methods(false), [], 'ObjectMixin class false')

test(Object.new.singleton_methods(true), [],  'ObjectMixin instance true')
test(Object.new.singleton_methods(false), [], 'ObjectMixin instance false')

report

Gemstone.abortTransaction if defined? RUBY_ENGINE
