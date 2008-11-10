class Fixnum

#  The 3 selectors  + - *   should be emitted as special sends and don't need
#  methods installed. They will fall back to Bignum (to implementations in
#  Smalltalk Integer) if the special send fails.

    primitive_nobridge '<'
    primitive_nobridge '>'
    primitive_nobridge '<='
    primitive_nobridge '>='

#  /    # note division does not produce Fractions in Ruby
	primitive_nobridge '/', '_rubyDivide:'

#   Ruby  %   maps to  Smalltalk #'\\' 
	primitive_nobridge '%', '\\\\'

	primitive_nobridge '**' , 'raisedTo:'

# unaries  +@  -@  eliminated during IR generation by compiler

	primitive_nobridge '|', 'bitOr:'
	primitive_nobridge '&', 'bitAnd:'
	primitive_nobridge '^', 'bitXor:'
	primitive_nobridge '<<', 'bitShift:'
	primitive_nobridge '>>', '_bitShiftRight:'

	primitive_nobridge '<=>', '_rubyCompare:'
	primitive_nobridge '[]', 'bitAt:'
	primitive 'abs', 'abs'

	primitive 'div', '//'
# divmod inherited from Numeric

	primitive 'id2name', '_ruby_id2name'

#    modulo   maps to Smalltalk  #'\\' 
	primitive 'modulo', '\\\\'

	primitive_nobridge 'quo', '_rubyQuo:'
	primitive 'size', '_rubySize'
	primitive 'to_f', 'asFloat'

        # to_s inherited from Integer

# TODO   to_sym

	primitive 'zero?', '_rubyEqualZero'
	primitive 'nonzero?', '_rubyNonzero'

  def self.name
    # override Smalltalk name
    'Fixnum'
  end

end
