require File.expand_path('simple', File.dirname(__FILE__))

class ArgsTest

  def test_000
    raise "Fail 1A" unless required_splat_no_block(1)       == 1
    raise "Fail 1B" unless required_splat_no_block(1, *[])  == 1
    raise "Fail 1C" unless required_splat_no_block(1, *[1]) == 2

    raise "Fail 2A" unless required_splat_with_block(1) { nil }       == 1
    raise "Fail 2B" unless required_splat_with_block(1, *[]) { nil }  == 1
    raise "Fail 2C" unless required_splat_with_block(1, *[1]) { nil } == 2


    # Call method taking optional parameter w/o passing optional parameter
    raise "Fail 3A" unless required_optional_splat_no_block(1) == 1
    raise "Fail 3B" unless required_optional_splat_no_block(1, *[]) == 1
    a = required_optional_splat_no_block(1, [1])
    raise "Fail 3C" unless a == 2

    # Call method taking optional parameter passing optional parameter
    raise "Fail 3D" unless required_optional_splat_no_block(1, "f") == 2
    raise "Fail 3E" unless required_optional_splat_no_block(1, "f", *[]) == 2
    raise "Fail 3F" unless required_optional_splat_no_block(1, "f", [1]) == 3

    # Call method taking optional parameter and block w/o passing optional
    # parameter
    raise "Fail 3G" unless
      required_optional_splat_no_block(1) { nil } == 1
    raise "Fail 3H" unless
      required_optional_splat_no_block(1, *[]) { nil } == 1
    raise "Fail 3I" unless
      required_optional_splat_no_block(1, [1]) { nil } == 2

    # Call method taking optional parameter and block passing optional
    # parameter
# TODO: These fail under MRI with a RuntimeError...
#    raise "Fail 3J" unless
#      required_optional_splat_with_block(1, "f") { nil } == 2
#    raise "Fail 3K" unless
#      required_optional_splat_with_block(1, "f", *[]) { nil } == 2
#    raise "Fail 3L" unless
#      required_optional_splat_with_block(1, "f", [1]) { nil } == 3
  end

  def test_00_no_block_with_splat_and_hard_coded_params
    m = "test_00_no_block_with_splat_and_hard_coded_params"

    actual = required_splat_no_block("an arg")
    expected = 1
    msg = "#{m}: No params: expected #{expected} actual: #{actual}"
    raise msg if expected != actual

    actual = required_splat_no_block("an arg", "optional 1")
    expected = 2
    msg = "#{m}: No params: expected #{expected} actual: #{actual}"
    raise msg if expected != actual

    actual = required_splat_no_block("an arg", "optional 1", "optional 2")
    expected = 3
    msg = "#{m}: No params: expected #{expected} actual: #{actual}"
    raise msg if expected != actual
  end

  def test_01_no_block_with_splat
    m = "test_01_no_block_with_splat: Wrong number of args"
    args = []

    3.times do |i|
      expected = 1 + args.length
      actual = required_splat_no_block("an arg", *args) # splat
      msg = "#{m}: expected #{expected} actual: #{actual} args: #{args.inspect}"
      raise msg if expected != actual
      args << i
    end
  end

  def test_02_no_block_without_splat
    m = "test_02_no_block_without_splat: Wrong number of args"
    args = []
    expected = 2

    3.times do |i|
      actual = required_splat_no_block("an arg", args) # no splat
      msg = "#{m}: expected #{expected} actual: #{actual} args: #{args.inspect}"
      raise msg if expected != actual
      args << i
    end
  end

  def test_03_no_block_with_splat_and_nils
    m = "test_03_no_block_with_splat_and_nils: Wrong number of args"
    args = []

    3.times do |i|
      expected = 1 + args.length
      actual = required_splat_no_block("an arg", *args) # splat
      msg = "#{m}: expected #{expected} actual: #{actual} args: #{args.inspect}"
      raise msg if expected != actual
      args << nil
    end
  end

  def test_04_no_block_without_splat_and_nils
    m = "test_04_no_block_without_splat_and_nils"
    args = []
    expected = 2

    3.times do |i|
      actual = required_splat_no_block("an arg", args) # no splat
      msg = "#{m}: expected #{expected} actual: #{actual} args: #{args.inspect}"
      raise msg if expected != actual
      args << nil
    end
  end

  def test_03_noextra
    x = required_three(10, 100, 1000)
    unless x = 1110 ; raise 'error' ; end
    y = 0
    begin
      x = required_three(1,2,3,4)
      y = 1
    rescue ArgumentError
      y = 5
    end
    unless y == 5; raise 'error' ; end;
    puts "test_03_noextra done"
  end

  def test_01_noextra
    x = required_one(5)
    unless x == 105 ; raise 'error' ; end
    a = [6];
    x = required_one(*a)
    unless x == 106 ; raise 'error' ; end
    a = [6,7];
    begin
      x = required_one(*a)
    rescue  ArgumentError
      x = 88
    end
    unless x == 88 ; raise 'error' ; end
    puts "test_01_noextra done"
  end

  ############## Tests with blocks


  ########### Helper methods that are called during testing

  def required_splat_no_block(required, *params)
    return 1 + params.size
  end

  def required_splat_with_block(required, *params, &block)
    return 1 + params.size
  end

  def required_optional_splat_no_block(required, optional=nil, *params)
    return 1 + (optional.nil? ? 0 : 1) + params.size
  end

  def required_optional_splat_with_block(required, optional=nil, *params, &block)
    return 1 + params.size
  end

  def required_three(a, b, c)
    return a + b + c
  end

  def required_one(a)
    return a + 100
  end
end


puts "Testing...."
tests = ArgsTest.new
tests.test_000
tests.test_00_no_block_with_splat_and_hard_coded_params
tests.test_01_no_block_with_splat
tests.test_02_no_block_without_splat
tests.test_03_no_block_with_splat_and_nils
tests.test_04_no_block_without_splat_and_nils
tests.test_03_noextra
tests.test_01_noextra

# Tests from "The Ruby Programming Language", section 6.4

def suffix(s, index=s.size-1)
  s[index,s.size-index]
end

test(suffix("Ruby"), "y", "Gemstone args A")
test(suffix("Ruby", 2), "by", "Gemstone args B")

# Ensure parameter defaults are evaluated when a method is invoked, rather
# than when it is parsed.  I.e., a should be a fresh array each time
# through...
def append(x, a=[])
  a << x
end

test(append(1), [1], "Gemstone args C")
test(append(1), [1], "Gemstone args D") # again to ensure a is fresh
test(append(1, [2]), [2,1], "Gemstone args E")

def sequence(args)
  n = args[:n] || 0
  m = args[:m] || 1
  [n,m]
end

test(sequence({ }), [0,1], "Gemstone args F")
test(sequence(:m => 3), [0,3], "Gemstone args G")
test(sequence(:m => 3, :n => 9), [9,3], "Gemstone args H")

# Next 3 tests inspired by a bug fix for String#split.  The Rubinius
# code used the first version (splat), the bug fix usees Array#concat
# instead.  Update: Array#push implementation was wrong and now fixed.

ret = ["A", "B"]
other_ary = ["FOO", nil, "BAR", nil]
test(ret.push(*other_ary.compact), ["A", "B", "FOO", "BAR"], "Regression 1")

ret = ["A", "B"]
other_ary = ["FOO", nil, "BAR", nil]
test(ret.push(*(other_ary.compact)), ["A", "B", "FOO", "BAR"], "Regression 2")

ret = ["A", "B"]
other_ary = ["FOO", nil, "BAR", nil]
test(ret.concat(other_ary.compact),["A", "B", "FOO", "BAR"], "Regression 3")

# coverage for Trac328
def meth328(*x)
    unless x.class.equal?(Array) ; raise 'error'; end
    unless x[0] = 3280 ; raise 'error'; end
    3281
end

[3280].each do | v |
  x = meth328(*v)
  unless x == 3281 ; raise 'error'; end
end
# end Trac328

# Test that passing with splat will call to_a and/or to_ary if available to
# coerce arg.
#
ret = []
x = "A string"
test(ret.push(*x), ['A string'], 'Non Array: does not respond to to_a, to_ary')

def x.to_a() ; [1,2]; end
ret.clear
test(ret.push(*x), [1,2], 'Non Array: responds to: to_a')

y = ''
def y.to_ary() ; [7,8]; end
ret.clear
test(ret.push(*y), [7,8], 'Non Array: responds to: to_ary')

def x.to_ary() ; [3,4]; end
ret.clear
test(ret.push(*x), [3,4], 'Non Array: responds to to_a, to_ary')

class CA
  # from language/def_spec.rb
  def bar(a=b=c=1,d=2)
    [a,b,c,d]
  end
  def foo(x = ($foo_self = self; nil))
    99
  end

  def self.test
    o = self.new
    x = [ o.bar() , o.bar(10), o.bar(10,20) ]
    o.foo
    y = $foo_self
    unless x == [[1, 1, 1, 2],    [10, nil, nil, 2],  [10, nil, nil, 20]]
      raise 'fail'
    end
    unless y._equal?(o)
      raise 'fail'
    end
    true
  end
end

CA.test

report

true
