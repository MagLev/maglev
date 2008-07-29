class Fixnum

#  The 3 selectors  + - *   should be emitted as special sends and don't need
#  methods installed. They will fall back to Bignum (to implementations in
#  Smalltalk Integer) if the special send fails.

    primitive '<'
    primitive '>'
    primitive '<='
    primitive '>='

#  /    # note division does not produce Fractions in Ruby
	primitive '/', '_rubyDivide:'

#   Ruby  %   maps to  Smalltalk #'\\' 
	primitive '%', '\\\\'

	primitive '**' , 'raisedTo:'

# unaries  +@  -@  eliminated during IR generation by compiler

	primitive '|', 'bitOr:'
	primitive '&', 'bitAnd:'
	primitive '^', 'bitXor:'
	primitive '<<', 'bitShift:'
	primitive '>>', '_bitShiftRight:'

	primitive '<=>', '_rubyCompare:'
	primitive '[]', 'bitAt:'
	primitive 'abs', 'abs'

	primitive 'div', '//'
# divmod inherited from Numeric

	primitive 'id2name', '_ruby_id2name'

#    modulo   maps to Smalltalk  #'\\' 
	primitive 'modulo', '\\\\'

	primitive 'quo', '_rubyQuo:'
	primitive 'size', '_rubySize'
	primitive 'to_f', 'asFloat'

# TODO   to_s
# TODO   to_sym

	primitive 'zero?', '_rubyEqualZero'
	primitive 'nonzero?', '_rubyNonzero'
end