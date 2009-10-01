require File.expand_path('simple', File.dirname(__FILE__))

# This file runs the MAGLEV_FAILURES data from testMarshal.rb



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
MAGLEV_FAILURES.each do |description, (object, marshal, attrs)|
  s = Marshal.dump(object)
  test(s, marshal, description)
end

report
true
