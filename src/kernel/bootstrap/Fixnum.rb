class Fixnum
  # Fixnum is identically  Smalltalk SmallInteger

  #  The 3 selectors  + - *   should be emitted as special sends and don't need
  #  methods installed. They will fall back to Bignum (to implementations in
  #  Smalltalk Integer) if the special send fails.

  # The selectors + - * >= <= < bitAnd: are special sends and may not
  #  be reimplemented in Fixnum.

  primitive_nobridge '_isSpecial', 'isSpecial'

  primitive '<',  '_rubyLt:'
  primitive '<=', '_rubyLteq:'
  primitive '>' , '_rubyGt:'
  primitive '>=', '_rubyGteq:'
  primitive '==', '_rubyEqual:'

  #  /    # note division does not produce Fractions in Ruby
  primitive_nobridge '/', '_rubyDivide:'

  #   Ruby  %   maps to  Smalltalk #'\\'
  primitive_nobridge '%', '_rubyModulus:'

  primitive_nobridge '**' , '_rubyRaisedTo:'

  # unaries  +@  -@  eliminated during IR generation by compiler

  primitive_nobridge '~', 'bitInvert'
  primitive_nobridge '&', '_rubyBitAnd:'
  primitive_nobridge '|', '_rubyBitOr:'
  primitive_nobridge '^', '_rubyBitXor:'
  primitive_nobridge '<<', '_rubyShiftLeft:'
  # >> inherited from Integer

  primitive_nobridge '<=>', '_rubyCompare:'
  primitive_nobridge '[]', 'bitAt:'
  primitive 'abs', 'abs'

  alias div /

  primitive 'id2name', '_ruby_id2name'

  alias modulo %

  # quo inherited from Integer
  primitive 'size', '_rubySize'
  primitive 'to_f', 'asFloat'

  # to_s inherited from Integer

  # TODO   to_sym

  primitive 'zero?', '_rubyEqualZero'
  primitive 'nonzero?', '_rubyNonzero'

end
