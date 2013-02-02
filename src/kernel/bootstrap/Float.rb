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

  primitive_nobridge '__class', 'class'  # returns SmallDouble

  primitive_nobridge '__isSpecial', 'isSpecial'
end

class Float

  def self.superclass
    Numeric  # override to hide Smalltalk BinaryFloat 
  end 

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
  MIN_10_EXP = -308
  MIN_EXP  = -1022 
  NaN      = __resolve_smalltalk_global(:PlusQuietNaN)
  RADIX    = 2
  ROUNDS   = 1  # towards nearest representable value,
     #  there is no support for changing rounding mode in the VM.

  PlusInfinity = __resolve_smalltalk_global(:PlusInfinity)
  MinusInfinity = __resolve_smalltalk_global(:MinusInfinity)

  def dup  
    raise TypeError , 'cannot dup a Float'
  end
  def clone  
    raise TypeError , 'cannot clone a Float'
  end

  def coerce(param)
    if param._isInteger
      return [ param.to_f, self]
    end
    begin
      if param._isNumeric
        v = param.to_f
      elsif param._isString
        v = Float(param)
      end
      if v._isFloat && ! v.nan?
        return [ v, self ]
      end
    rescue
        # continue execution
    end
    super(param)
  end

  def self.induced_from(obj)
    if obj._isFloat
      obj
    elsif obj._isInteger
      Maglev::Type.coerce_to(obj, Float, :to_f)
    else
      raise TypeError, "arg to induce_from neither Float nor Integer"
      nil
    end 
  end

  primitive_nobridge '+', '_rubyAdd:'
  primitive_nobridge '-', '_rubySubtract:'
  primitive_nobridge '*', '_rubyMultiply:'
  primitive_nobridge '/', '_rubyDivide:'
  primitive_nobridge '__divide', '_rubyDivide:'
  primitive_nobridge '%', '_rubyModulo:'
  primitive_nobridge 'modulo', '_rubyModulo:'
  primitive_nobridge '__kind', '_getKind'

  def div(arg)
    q = self.__divide(arg)
    q.floor
  end

  def divmod(arg)
    unless arg._isNumeric
      raise TypeError, 'arg to divmod is not a Numeric'
    end
    a = Maglev::Type.coerce_to(arg, Float, :to_f)
    unless (sk = self.__kind)._equal?(1)  # Not normal
      unless sk._equal?(1) || sk._equal?(4)  # neither subnormal nor zero
        raise FloatDomainError, 'receiver of divmod infinite or nan' 
      end
    end
    if a.__kind >= 4 
      raise FloatDomainError ,'arg to divmod was zero or NaN'
    end
    [ (self.__divide(a)).floor , self % a  ]
  end

  # quo inherited from Numeric

  primitive_nobridge '__raised_to', '_rubyRaisedTo:'
  def **(arg)
    unless arg._isFloat
      if arg._isInteger
        arg = arg.to_f
      else
        c = arg.coerce(self)
        return c[0] ** c[1] 
      end
    end
    self.__raised_to(arg)
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
    if arg._isFloat
      if self < arg
	-1 
      elsif self == arg
	0 
      elsif self.finite? and arg.finite?
	1
      else
        nil
      end
    elsif arg._isInteger
      if self.finite? 
        a = Maglev::Type.coerce_to(arg, Float, :to_f)
        self <=> a
      elsif self.infinite?
        if self > 0.0
          1
        else
          -1
        end 
      else
        nil
      end
    elsif arg._isNumeric
      a = Maglev::Type.coerce_to(arg, Float, :to_f)
      self <=> a
    else
      nil
    end
  end

  def abs
    if self < 0.0
      - self
    else
      self
    end
  end

  primitive_nobridge 'ceil', 'ceiling'

  primitive_nobridge 'eql?', '_ruby_eqlQ:'

  primitive_nobridge 'finite?', '_ruby_finiteQ'
  primitive_nobridge 'floor', 'floor'
  primitive_nobridge 'hash'
  primitive_nobridge 'infinite?', '_ruby_infiniteQ'
  primitive_nobridge '__sign', '_sign'  # considers -0.0 to be negative

  primitive_nobridge 'nan?', '_isNaN'
  primitive_nobridge 'round', 'rounded'
  primitive_nobridge 'to_f' , 'asFloat'
  primitive_nobridge '__to_float' , 'asFloat'
  primitive_nobridge 'to_i' , 'truncated'
  primitive_nobridge 'to_int' , 'truncated'
  primitive_nobridge 'to_s' , '_rubyAsString'  
  primitive_nobridge '__as_string' , 'asString'   # Smalltalk format sd.d...dEsee
  primitive          'inspect' , '_rubyAsString'  
  primitive_nobridge 'truncate' , 'truncated'

  # Note: nonstandard meth to format Float - for use by Benchmark 
  primitive 'to_fmt' , '_rubyAsFormattedString' 

  def zero?
    self == 0.0
  end

  def nonzero?
    if self == 0.0
      nil
    else
      self
    end
  end
    
#  methods from Numeric
  primitive_nobridge 'floor', 'floor'

  def step(nend, inc=1.0 , &block)
    inc = Maglev::Type.coerce_to(inc, Float, :to_f)
    n = self
    if inc == 0.0
      raise ArgumentError, "increment is zero"
    end
    nend = Maglev::Type.coerce_to(nend, Float, :to_f)
    unless block_given?
      return NumericEnumerator.new(self, self, nend, inc) # for 1.8.7
    end
    if (inc > 0.0)
      if (nend == PlusInfinity)
        block.call(n)  # yield once
      else 
        while n <= nend
	  block.call(n)
	  n += inc
        end
      end
    else  
      if nend == MinusInfinity
        block.call(n)  # yield once
      else
	while n >= nend
	  block.call(n)
	  n += inc
	end
      end
    end
  end


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
  primitive_nobridge '__atan2', 'arcTan2:'
  primitive_nobridge '__hypot', 'hypot:'
  primitive_nobridge  '__ldexp', 'ldexp:'
end
Float.__freeze_constants

