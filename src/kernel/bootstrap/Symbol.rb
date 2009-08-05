#---------------------------------
# Symbol
#    Smalltalk Symbol and Ruby Symbol are identical classes
#    note in Ruby , superclass of Symbol is Object

class Symbol
  # returns an Array containing all keys in the Smalltalk dictionary AllSymbols
  class_primitive_nobridge 'all_symbols', '_rubyAllSymbols'

  def self.superclass
    Object  # override because Smalltalk would return String
  end

  primitive_nobridge 'id2name', 'asString'
  primitive_nobridge '==', '='
  primitive_nobridge 'hash'

  # _concatenate inherited from String for now
  def inspect
    ':'._concatenate(self)
  end

  # You may not create symbols directly
  def self.new
    raise 'Illegal creation of a Symbol'
  end
  def initialize
    raise 'Illegal construction of a Symbol'
  end

  # You may not copy Symbols
  def dup
    self
  end
  def clone
    self
  end

  primitive_nobridge 'to_i', 'asOop'
  primitive_nobridge 'to_int', 'asOop'
  primitive_nobridge 'to_s', 'asString'
  primitive_nobridge 'to_sym', 'asSymbol'
  primitive_nobridge 'exception', 'asRubyException'

  def call(value)
    value.__send__(self)
  end

  def respond_to?(sym)
    if sym.equal?(:to_str)
      false
    else
      super(sym)
    end
  end

  def to_str
    # MRI raises a NoMethodError
    raise TypeError , 'to_str not allowed for Symbol'
  end
end
