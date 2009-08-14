# From Rubinius: experimental 1.8.7 / 1.9 support
# A class which provides a method `each' to be used as an Enumerable
# object.

module Enumerable
  class Enumerator
    include Enumerable

    def initialize(obj,iter, *args)
      @object = obj
      @iter = iter
      @iterator = @object.method(iter.to_sym)
      @args = args
    end

    def each(&block)
      @iterator.call(*@args, &block)
    end
  end

  def each_cons(arg, &block)
    n = Type.coerce_to( arg, Integer, :to_int)
    unless n > 0
      raise TypeError, 'enum_cons, arg must be > 0'
    end
    array = []
    elements = self.to_a
    while elements.size > 0 do
      elem = elements[0,n]
      array << elem if elem.size == n
      elements.shift
    end
    res = array.each { |set| yield set }
    if res.equal?(array)
      nil
    else
      res # the &block did a break
    end
  end

  def enum_cons(arg)
    Enumerable::Enumerator.new(self, :each_cons, n)
  end

  def each_slice(arg, &block)
    slice_size = Type.coerce_to( arg, Integer, :to_int)
    unless slice_size > 0
      raise TypeError, 'each_slice, arg must be > 0'
    end
    a = []
    ea_res = self.each { |element|
      a << element
      if a.length == slice_size
        yield a
        a = []
      end
    }
    if ea_res.equal?(self)
      yield a if a.length > 0
      nil
    else
      ea_res # the &block did a break
    end
  end

  def enum_slice(n)
    Enumerable::Enumerator.new(self, :each_slice, n)
  end

  def enum_with_index
    Enumerable::Enumerator.new(self, :each_with_index)
  end
end

class Object
  def enum_for(method, *args)
    Enumerable::Enumerator.new(self,method,*args)
  end
end
