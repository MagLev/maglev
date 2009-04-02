# ---------------------------------
#  Float ,  The Ruby and Smalltalk  Float classes are identical
#

class SmallDouble
  def self.name
    'Float'
  end

  def class
    Float
  end

  primitive_nobridge '_isSpecial', 'isSpecial'
end

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
  NaN      = _resolve_smalltalk_global(:PlusQuietNaN)
  RADIX    = 2
  ROUNDS   = 1  # towards nearest representable value,
     # ROUNDS is made invariant by code in RubyContext 
     #  because there is no support for changing rounding mode in the VM.

  def coerce(param)
    begin
      v = param.to_f
      if v._isFloat
        return [ v, self ]
      end
    rescue
      # continue execution
    end
    super
  end

  def self.induced_from(obj)
    if obj._isFloat
      obj
    elsif obj._isInteger
      Type.coerce_to(obj, Float, :to_f)
    else
      raise TypeError, "arg to induce_from neither Float nor Integer"
      nil
    end 
  end

  primitive_nobridge '+', '_rubyAdd:'
  primitive_nobridge '-', '_rubySubtract:'
  primitive_nobridge '*', '_rubyMultiply:'
  primitive_nobridge '/', '_rubyDivide:'
  primitive_nobridge '_divide', '_rubyDivide:'
  primitive_nobridge '%', '_rubyModulo:'
  primitive_nobridge 'modulo', '_rubyModulo:'

  def div(arg)
    q = self._divide(arg)
    q.floor
  end

  def divmod(arg)
    unless arg._isNumber
      raise TypeError, 'arg to divmod is not a Numeric'
    end
    a = Type.coerce_to(arg, Float, :to_f)
    if a == 0.0
      raise FloatDomainError ,'arg to divmod was zero'
    end
    q = (self._divide(a)).floor
    r = self % a 
    unless (self < 0.0).equal?(a < 0.0)
      r = r + a
    end
    [ q, r ]
  end

  # quo inherited from Numeric

  primitive_nobridge '_raised_to', '_rubyRaisedTo:'
  def **(arg)
    a = Type.coerce_to(arg, Float, :to_f)
    self._raised_to(a)
  end

# unaries  +@  -@  eliminated during IR generation by compiler

  primitive_nobridge '<',  '_rubyLt:'
  primitive_nobridge '<=', '_rubyLteq:'

  def >(arg)
    arg < self
  end

  def >=(arg)
    arg <= self
  end

  primitive_nobridge '==', '_rubyEqual:'

  #  primitive '!=', '_rubyNotEqual:'

  def <=>(arg)
    # reimplemented for efficiency since Float > not a prim
    if arg._isNumber
      if self < arg
	-1 
      elsif self == arg
	0 
      else
	1 
      end
    else
      nil
    end
  end

  primitive_nobridge 'abs', 'abs'
  primitive_nobridge 'ceil', 'ceiling'

  primitive_nobridge 'eql?', '_ruby_eqlQ:'

  primitive_nobridge 'finite?', '_ruby_finiteQ'
  primitive_nobridge 'floor', 'floor'
  primitive_nobridge 'hash'
  primitive_nobridge 'infinite?', '_ruby_infiniteQ'

  primitive_nobridge 'nan?', '_isNaN'
  primitive_nobridge 'round', 'rounded'
  primitive_nobridge 'to_f' , 'asFloat'
  primitive_nobridge 'to_i' , 'truncated'
  primitive_nobridge 'to_int' , 'truncated'
  primitive_nobridge 'to_s' , '_rubyAsString'  
  primitive_nobridge 'truncate' , 'truncated'
  primitive_nobridge 'zero?' , '_rubyEqualZero'

# Note: nonstandard meth to format Float - for use by Benchmark 
primitive 'to_fmt' , '_rubyAsFormattedString' 
    
#  methods from Numeric
  primitive_nobridge 'floor', 'floor'
  primitive_nobridge 'nonzero?', '_rubyNonzero'

# trig methods used by Math
  primitive_nobridge 'acos', 'arcCos'
  primitive_nobridge 'acosh', 'arcCosh'
  primitive_nobridge 'asin', 'arcSin'
  primitive_nobridge 'asinh', 'arcSinh'
  primitive_nobridge 'atan', 'arcTan'
  primitive_nobridge 'atanh', 'arcTanh'

  primitive_nobridge 'cos', 'cos'
  primitive_nobridge 'cosh', 'cosh'
  primitive_nobridge 'erf', 'erf'
  primitive_nobridge 'erfc', 'erfc'
  primitive_nobridge 'exp', 'exp'
  primitive_nobridge 'frexp', 'frexp'
  primitive_nobridge 'log', 'ln'
  primitive_nobridge 'log2', 'log2'
  primitive_nobridge 'log10', 'log10'
  primitive_nobridge  'modf', 'modf'
  primitive_nobridge 'sin', 'sin'
  primitive_nobridge 'sinh', 'sinh'
  primitive_nobridge 'sqrt', 'sqrt'
  primitive_nobridge 'tan', 'tan'
  primitive_nobridge 'tanh', 'tanh'

  # following 3 not intented for public use, 
  #  coercion of arguments is done in Math.rb
  primitive_nobridge '_atan2', 'arcTan2:'
  primitive_nobridge '_hypot', 'hypot:'
  primitive_nobridge  '_ldexp', 'ldexp:'
end
