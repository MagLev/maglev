#---------------------------------
# Symbol
#    Smalltalk Symbol and Ruby Symbol are identical classes .
#    In Smalltalk, the superclass of Symbol is String .
#
#    You cannot code  'class Symbol < Object' in this file
#    because Object would not match the Smalltalk superclass.
#    In environments >= 1, at the end of bootstrapping,
#    the superclass of Symbol is changed to be Object by use
#    of RubyContext>>_fixSymbolSuperclass:  in the .mcz code.

class Symbol
  # returns an Array containing all keys in the Smalltalk dictionary AllSymbols
  class_primitive_nobridge 'all_symbols', '_rubyAllSymbols'

  class_primitive '__existing_symbol', '_existingWithAll:' # arg is a String

  primitive_nobridge 'id2name', 'asString'
  primitive_nobridge '==', '='  # uses Symbol>>= which is identity compare
  primitive_nobridge 'eql?', '='  # uses Symbol>>= which is identity compare
  primitive_nobridge 'hash'
  primitive '__at_equals', 'at:equals:' # one-based offset

  # allow String-like access to bytes of Symbols from bootstrap code 
  primitive_nobridge_env '__at' , '_rubyAt', ':' 

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
    raise TypeError , 'cannot dup a Symbol'
  end
  def clone  
    raise TypeError , 'cannot clone a Symbol'
  end

  primitive_nobridge 'to_i', 'asOop'
  primitive_nobridge 'to_int', 'asOop'
  primitive_nobridge 'to_s', 'asString'
  primitive_nobridge 'to_sym', 'asSymbol'
  primitive_nobridge 'exception', 'asRubyException'

  def taint
    self # do nothing
  end

  def to_proc
    Proc.new { |*args| args.shift.__send__(self, *args) }
  end

  def =~(obj)
    to_s =~ obj
  end

end
