# ---------------------------------
#  Float ,  The Ruby and Smalltalk  Float classes are identical
#
#  Smalltalk SmallDouble is a subclass of Float, so  we only have
#   to Bootstrap the env 1 method dictionary for Float .

class Float

  # Float constants 
      # changing any of the constants at runtime will only change the constant
      # value and will have no effect on on results of floating point computations.
  DIG      = 15		
  EPSILON  = 2.2204460492503131E-16  # smallest Float such that (1.0 + EPSLON) != 1.0
  MANT_DIG = 53
  MAX      = 1.7976931348623157E+308 # largest Float smaller than Infinity
  MAX_10_EXP = 308  
  MAX_EXP  = 1024
  MIN      = 2.2250738585072014E-308 # smallest positive float not a subnormal
  MIN_10_EXP = -307
  MIN_EXP  = -1021 
  RADIX    = 2
  ROUNDS   = 1  # towards nearest representable value,
                # ROUNDS is made invariant by code in RubyContext 
	        #  because there is no support for changing rounding mode in the VM.

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
