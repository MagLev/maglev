require File.expand_path('simple', File.dirname(__FILE__))

h = {'one' => {'two' => {'three' => 0}}}
b = Marshal.dump(h, 7);

hb = Marshal.load(b)
unless h == hb ; raise 'error' ; end
puts "Hash ok"

k = Class.new
a = 0
begin
  a = Marshal.dump(k)
rescue TypeError
  puts "ok a"
end

m = Module.new
b = 0
begin
  b = Marshal.dump(m)
rescue TypeError
  puts "ok b"
end

class Foo
  attr_reader :bar, :baz
  def initialize
    @bar = "hello"
    @baz = 1
  end
  def ==(other)
    other.class == self.class && other.bar == @bar && other.baz == @baz
  end
end

raise "nan"       unless Marshal.load(Marshal.dump(0.0/0.0)).nan?
raise "-infinity" unless Marshal.load(Marshal.dump(-1.0/0.0)).infinite? == -1
raise "infinity"  unless Marshal.load(Marshal.dump(1.0/0.0)).infinite? == 1

# This array was generated from MRI.  The strings are the MRI 1.8.6
# marshaled strings for the given object.
test_items = [
              [Foo.new, "\004\bo:\bFoo\a:\t@bazi\006:\t@bar\"\nhello"],
              [1..4, "\004\bo:\nRange\b:\nbegini\006:\texclF:\bendi\t"],
              [nil, "\004\b0"],
              [true, "\004\bT"],
              [false, "\004\bF"],
              [:a_symbol, "\004\b:\ra_symbol"],
              ["xyz", "\004\b\"\bxyz"],
              [1, "\004\bi\006"],
              [2.71828, "\004\bf\0172.71828\000\367\220"],
              #[/xyz/i, "\004\b/\bxyz\001"],
              [/xyz/, "\004\b/\bxyz\000"],
              [["hello", 12], "\004\b[\a\"\nhelloi\021"],
              [{"one"=>"two"}, "\004\b{\006\"\bone\"\btwo"],
#              [18888888888888888888888888888888888, "\004\bl+\r8\216\3438\022\324'\367\243\216\271KK\243\003\000"],
#               Struct
             ]

test_items.each do |(item, marshal)|
  b = Marshal.dump(item)
  item2 = Marshal.load(b)
  #puts "=== #{item.inspect} => #{item2.inspect}"
  test(item2, item, "Unmarshal: item: #{item.inspect} item2: #{item2.inspect}")

  item2 = Marshal.load(marshal)
  test(item2, item, "Unmarshal MRI: item: #{item.inspect} item2: #{item2.inspect}")
end

# The following class defns and the DATA array are taken from the rubyspecs
# Marshal spec.  The mock should_equal seems to mess up on these, so we
# test them here.

class UserDefined

  class Nested
    def ==(other)
      other.kind_of? self.class
    end
  end

  attr_reader :a, :b

  def initialize
    @a = 'stuff'
    @b = @a
  end

  def _dump(depth)
    Marshal.dump [@a, @b]
  end

  def self._load(data)
    a, b = Marshal.load data

    obj = allocate
    obj.instance_variable_set :@a, a
    obj.instance_variable_set :@b, b

    obj
  end

  def ==(other)
    self.class === other and
    @a == other.a and
    @b == other.b
  end

end

class UserDefinedWithIvar
  attr_reader :a, :b, :c

  def initialize
    @a = 'stuff'
    @a.instance_variable_set :@foo, :UserDefinedWithIvar
    @b = 'more'
    @c = @b
  end

  def _dump(depth)
    Marshal.dump [@a, @b, @c]
  end

  def self._load(data)
    a, b, c = Marshal.load data

    obj = allocate
    obj.instance_variable_set :@a, a
    obj.instance_variable_set :@b, b
    obj.instance_variable_set :@c, c

    obj
  end

  def ==(other)
    self.class === other and
    @a == other.a and
    @b == other.b and
    @c == other.c and
    @a.instance_variable_get(:@foo) == other.a.instance_variable_get(:@foo)
  end
end

class UserMarshal
  attr_reader :data

  def initialize
    @data = 'stuff'
  end
  def marshal_dump() @data end
  def marshal_load(data) @data = data end
  def ==(other) self.class === other and @data == other.data end
end

class UserMarshalWithIvar
  attr_reader :data

  def initialize
    @data = 'my data'
  end

  def marshal_dump
    [@data]
  end

  def marshal_load(o)
    @data = o.first
  end

  def ==(other)
    self.class === other and
    @data = other.data
  end
end

class UserArray < Array
end

class UserHash < Hash
end

class UserHashInitParams < Hash
  def initialize(a)
    @a = a
  end
end

class UserObject
end

class UserRegexp < Regexp
end

class UserString < String
end

module Meths
  def meths_method() end
end

module MethsMore
  def meths_more_method() end
end

Struct.new "Pyramid"
Struct.new "Useful", :a, :b

DATA = {
    "nil" => [nil, "\004\b0"],
    "Struct" => [Struct::Useful.new(1, 2),
                 "\004\bS:\023Struct::Useful\a:\006ai\006:\006bi\a"],
    "Symbol" => [:symbol,
                 "\004\b:\vsymbol"],
    "true" => [true,
               "\004\bT"],
    "false" => [false,
                "\004\bF"],
    "String empty" => ['',
                       "\004\b\"\000"],
    "String small" => ['small',
                       "\004\b\"\012small"],
    "String big" => ['big' * 100,
                     "\004\b\"\002,\001#{'big' * 100}"],
    "String subclass" => [UserString.new,
                          "\004\bC:\017UserString\"\000"],
    "Symbol small" => [:big,
                       "\004\b:\010big"],
    "Symbol big" => [('big' * 100).to_sym,
                               "\004\b:\002,\001#{'big' * 100}"],
    "Fixnum -2**24" => [-2**24,
                        "\004\bi\375\000\000\000"],
    "Fixnum -2**16" => [-2**16,
                        "\004\bi\376\000\000"],
    "Fixnum -2**8" => [-2**8,
                       "\004\bi\377\000"],
    "Fixnum -123" => [-123,
                      "\004\bi\200"],
    "Fixnum 0" => [0,
                   "\004\bi\000"],
    "Fixnum 5" => [5,
                   "\004\bi\n"],
    "Fixnum 2**8" => [2**8,
                      "\004\bi\002\000\001"],
    "Fixnum 2**16" => [2**16,
                       "\004\bi\003\000\000\001"],
    "Fixnum 2**24" => [2**24,
                       "\004\bi\004\000\000\000\001"],
    "Class String" => [String,
                       "\004\bc\vString"],
    "Module Marshal" => [Marshal,
                         "\004\bm\fMarshal"],
    "_dump object extended" => [UserDefined.new.extend(Meths),
                                "\004\bu:\020UserDefined\022\004\b[\a\"\nstuff@\006\000"],
    "marshal_dump object" => [UserMarshalWithIvar.new,
                              "\004\bU:\030UserMarshalWithIvar[\006\"\fmy data"],
    "Float 0.0" => [0.0,
                    "\004\bf\0060"],
    "Float -0.0" => [-0.0,
                     "\004\bf\a-0"],
    "Float Infinity" => [(1.0 / 0.0),
                         "\004\bf\binf"],
    "Float -Infinity" => [(-1.0 / 0.0),
                          "\004\bf\t-inf"],
    "Float 1.0" => [1.0,
                    "\004\bf\0061"],
    "Hash" => [Hash.new,
               "\004\b{\000"],
    "Hash subclass" => [UserHash.new,
                        "\004\bC:\rUserHash{\000"],
    "Array" => [Array.new,
                "\004\b[\000"],
    "Array subclass" => [UserArray.new,
                     "\004\bC:\016UserArray[\000"],
    "Struct" => [Struct::Pyramid.new,
                 "\004\bS:\024Struct::Pyramid\000"],
}


# Maglev failures =
MAGLEV_FAILURES = {
    "String extended" => [''.extend(Meths), # TODO: check for module on load
                          "\004\be:\nMeths\"\000"],
    "String subclass extended" => [UserString.new.extend(Meths),
                                   "\004\be:\nMethsC:\017UserString\"\000"],
    "Bignum -2**64" => [-2**64,
                        "\004\bl-\n\000\000\000\000\000\000\000\000\001\000"],
    "Bignum -2**63" => [-2**63,
                        "\004\bl-\t\000\000\000\000\000\000\000\200"],
    "Bignum 2**64" => [2**64,
                       "\004\bl+\n\000\000\000\000\000\000\000\000\001\000"],
    "Bignum 2**90" => [2**90,
                       "\004\bl+\v#{"\000" * 11}\004"],
    "1...2" => [(1...2),
                "\004\bo:\nRange\b:\nbegini\006:\texclT:\bendi\a",
               { :begin => 1, :end => 2, :exclude_end? => true }],
    "Module nested" => [UserDefined::Nested.new,
                        "\004\bo:\030UserDefined::Nested\000"],
    "Regexp subclass /i" => [UserRegexp.new('', Regexp::IGNORECASE),
                             "\004\bC:\017UserRegexp/\000\001"],
    "'a'..'b'" => [('a'..'b'),
                   "\004\bo:\nRange\b:\nbegin\"\006a:\texclF:\bend\"\006b",
                   { :begin => 'a', :end => 'b', :exclude_end? => false }],
    "1..2" => [(1..2),
               "\004\bo:\nRange\b:\nbegini\006:\texclF:\bendi\a",
               { :begin => 1, :end => 2, :exclude_end? => false }],
    "Regexp" => [/\A.\Z/,
                 "\004\b/\n\\A.\\Z\000"],
    "_dump object" => [UserDefinedWithIvar.new,
                       "\004\bu:\030UserDefinedWithIvar5\004\b[\bI\"\nstuff\006:\t@foo:\030UserDefinedWithIvar\"\tmore@\a"],
}
DATA.each do |description, (object, marshal, attrs)|
  s = Marshal.dump(object)
  test(s, marshal, description)
end

# Distilled from a problem running Rails
#
# Marshal.load(data) was raising a NameException because it was trying to do
# Kernel.const_defined?(:'ActionDispatch::Flash::FlashHash').  The bug fix
# was to call #get_scoped_constant() instead of #const_get.
require 'set'
module ActionDispatch
  module Flash
    class FlashHash < Hash

      # Bug 2:
      # A second during marshal loading was that "flash_hash[key] = value"
      # was being used to populate the underlying Hash.  But, FlashHash#[]=
      # referenced an as of yet uninitialized inst var, @used, and failed.
      # Now the marshal code calls Hash#__atkey_put to "avoid" method
      # lookup.  So, if the above passes, then this second test also
      # passes.
      def []=(k,v)
        raise "Fail: FlashHash#[]= called during marshal..."
      end

    end
  end
end

data = "\004\b{\b\"\020_csrf_token\"1XcuLKI+cUanbHaOw4N9PEgLRl/BKd4MAPeC4+9JB9Ws=\"\017session_id\"%72a38c12f9143f3c248f9706edfcc4e0\"\nflashIC:%ActionDispatch::Flash::FlashHash{\006:\vnotice\"#Post was successfully created.\006:\n@usedo:\bSet\006:\n@hash{\006;\006T"

obj = Marshal.load(data)
# testing two birds with one test case...
test(obj['flash'], { :notice => "Post was successfully created."}, "Rails Test")


# This is the same test case, but for arrays:
class XArray < Array
  def <<(item)
    super
    raise "Fail: XArray#<< called during marshal..."
  end
end

xa = XArray.new
10.times { |i| xa[i] = i }  # avoid << !

serialized = Marshal.dump(xa)
xa2 = Marshal.load(serialized)
test(xa, xa2, 'Marshal avoids Array#<< for  subclasses')

report
true
