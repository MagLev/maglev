#---------------------------------
# Symbol
#    Smalltalk Symbol and Ruby Symbol are identical classes
#    note in Ruby , superclass of Symbol is Object

class Symbol
  class_primitive_nobridge 'all_symbols', '_rubyAllSymbols'
  primitive_nobridge 'id2name', 'asString'
  primitive_nobridge '==', '='
  primitive_nobridge 'hash'
  primitive_nobridge 'inspect', 'asSymbol'
  primitive_nobridge 'to_i', 'asOop'
  primitive_nobridge 'to_int', 'asOop'
  primitive_nobridge 'to_s', 'asString'
  primitive_nobridge 'to_sym', 'asSymbol'


  def call(value)
    value.__send__(self)
  end
end
