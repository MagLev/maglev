# Trac
#
# MagLev was calling method_added with the non-stripped version of the
# selector, e.g., "foo:&".  This test passes if it doesn't raise an
# exception.

# Test instance methods
class C
  def self.method_added(meth)
     raise "FAIL: #{self}.method_added( #{meth} )" if meth.to_s =~ /[:*&]/
  end
end

class C
  def m1
  end
  def m2(&block)
  end
  def m3(*args)
  end
  def m4(x)
  end
  def m5(x=nil)
  end
end

# Test class/singleton methods
class C
  class << self
    def self.method_added(meth)
      name = meth.to_s
      raise "FAIL: #{self}.method_added( #{meth} )" if meth.to_s =~ /[:*&]/
    end
  end
end

class C
  class << self
    def mc1
    end
    def mc2(&block)
    end
    def mc3(*args)
    end
    def mc4(x)
    end
    def mc5(x=nil)
    end
  end
end
