# ---------------------------------
#  Float ,  The Ruby and Smalltalk  Float classes are identical
#
#  Smalltalk SmallDouble is a subclass of Float, so  we only have
#   to Bootstrap the env 1 method dictionary for Float .

class Float
	primitive '+', '+'
	primitive '-', '-'
	primitive '*', '*'
	primitive '/', '/'

#    %  maps to  Smalltalk  #'\\' , will use the implementation in Number
	primitive '%', '\\\\'

#   ** uses   raiseTo:  which  coerces argument to a Float first .
	primitive '**', 'raisedTo:'

# unaries  +@  -@  eliminated during IR generation by compiler

	primitive '<=>', '_rubyCompare:'
	primitive '<'
	primitive '<='
	primitive '>'
	primitive '>='
	primitive '==', '='

	primitive 'abs', 'abs'
	primitive 'ceil', 'ceiling'

	primitive 'divmod', '_divmod:'

	primitive 'eql?', '_ruby_eqlQ:'

	primitive 'finite?', '_ruby_finiteQ'
	primitive 'floor', 'floor'
	primitive 'hash'
	primitive 'infinite?', '_ruby_infiniteQ'

#    modulo   maps to Smalltalk  #'\\' 
	primitive 'modulo' , '\\\\'

	primitive 'nan?', '_isNaN'
	primitive 'round', 'rounded'
	primitive 'to_f' , 'asFloat'
	primitive 'to_i' , 'truncated'
	primitive 'to_int' , 'truncated'
	primitive 'to_s' , '_rubyAsString'  
	primitive 'truncate' , 'truncated'
	primitive 'zero?' , '_rubyEqualZero'
  
# Note: nonstandard meth to format Float - for use by Benchmark 
  primitive 'to_fmt' , '_rubyAsFormattedString' 
	
#  methods from Numeric
	primitive 'coerce', '_rubyCoerce:'
	primitive 'floor', 'floor'
	primitive 'nonzero?', '_rubyNonzero'
end
