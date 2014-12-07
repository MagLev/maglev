require "delegate"

class WeakRef < Delegator
  SoftReference = __resolve_smalltalk_global(:SoftReference)

  class SoftReference
    primitive :set_value, :"setValue:"
    primitive :value, :_value
    class_primitive :new, :new
  end

  def initialize(value)
    super
    __setobj__(value)
  end

  def __setobj__(value)
    @softref = SoftReference.new
    @softref.set_value(value)
  rescue ArgumentError
    raise ArgumentError, "cannot set #{value.inspect} as weak reference"
  end

  def __getobj__
    @softref.value
  end

  def weakref_alive?
    __getobj__ != nil
  end

  def self.maglev_persistable
    raise TypeError, "cannot persist instances of WeakRef"
  end
end
