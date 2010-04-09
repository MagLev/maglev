# AModule.instance_methods was returning methods from Module, which should not happen.
# Plus, it seems, the protection field isn't right re protected.

require 'test/unit'

module AModule
  def foo ; 10; end

  private
  def bar ; 20; end

  protected
  def quux ; 30; end
end

class TestModuleMethods < Test::Unit::TestCase
  def test_empty_module
    m = Module.new
    [:instance_methods, :public_instance_methods,
     :private_instance_methods, :protected_instance_methods].each do |meth|
      assert_equal(m.send(meth),        [], "m.#{meth}() == []")
      assert_equal(m.send(meth, true),  [], "m.#{meth}(true) == []")
      assert_equal(m.send(meth, false), [], "m.#{meth}(false) == []")
    end
  end

  def test_normal_module
    [[:instance_methods,           ['foo', 'quux']],
     [:public_instance_methods,    ['foo']],
     [:private_instance_methods,   ['bar']],
     [:protected_instance_methods, ['quux']]
    ].each do |meth, expected|
      assert_equal(expected, AModule.send(meth).sort,
                   "AModule.#{meth}() == #{expected.inspect}")
      assert_equal(expected, AModule.send(meth, true).sort,
                   "AModule.#{meth}(true) == #{expected.inspect}")
      assert_equal(expected, AModule.send(meth, false).sort,
                   "AModule.#{meth}(false) == #{expected.inspect}")
    end
  end
end

class AClass
  def foo ; 10; end

  private
  def bar ; 20; end

  protected
  def quux ; 30; end
end

class TestClassMethods < Test::Unit::TestCase
  # Since the current number of methods in Object is still fluctuating a
  # bit, and it is a bit different between MRI and MagLev, we just need to
  # distinguish between the cases where "a bunch" of methods from Object
  # are included or not, so we have a range.
  BIG_RANGE = (30..100)
  LOW_RANGE = (0..4)
  NONE      = (0..0)
  ONE       = (1..1)
  def test_empty_class
    c = Class.new
    [[:instance_methods,           BIG_RANGE, NONE],
     [:public_instance_methods,    BIG_RANGE, NONE],
     [:private_instance_methods,   BIG_RANGE, NONE],
     [:protected_instance_methods, NONE,      NONE]].each do |meth,t_range, f_range|
      actual = c.send(meth)
      assert(t_range.include?(actual.size),
             "c.#{meth}():  Expected range: #{t_range} Actual: #{actual.size}")

      actual = c.send(meth,true)
      assert(t_range.include?(actual.size),
             "c.#{meth}(true):  Expected range: #{t_range} Actual: #{actual.size}")

      actual = c.send(meth,false)
      assert(f_range.include?(actual.size),
             "c.#{meth}(false):  Expected range: #{f_range} Actual: #{actual.size}")
    end
  end

  def test_normal_class
    [[:instance_methods,           BIG_RANGE, (2..2)],
     [:public_instance_methods,    BIG_RANGE, ONE],
     [:private_instance_methods,   BIG_RANGE, ONE],
     [:protected_instance_methods, ONE,       ONE]].each do |meth,t_range, f_range|
      actual = AClass.send(meth)
      assert(t_range.include?(actual.size),
             "AClass.#{meth}():  Expected range: #{t_range} Actual: #{actual.size}")

      actual = AClass.send(meth,true)
      assert(t_range.include?(actual.size),
             "AClass.#{meth}(true):  Expected range: #{t_range} Actual: #{actual.size}")

      actual = AClass.send(meth,false)
      assert(f_range.include?(actual.size),
             "AClass.#{meth}(false):  Expected range: #{f_range} Actual: #{actual.size}")
    end
  end
end
