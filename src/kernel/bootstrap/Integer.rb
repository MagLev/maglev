# ~
# ---------------------------------
#  Bignum and Integer
#
# The Smalltalk hierarchy is
#      Integer
#        LargeInteger
#        SmallInteger
#

class Integer

  def coerce(param)
    if param._isFloat
      s = self.to_f 
      if s._isFloat && ! s.nan?
        return [ param, s ]
      end
    elsif param._isInteger
      return [ param, self ]
    elsif param._isString
      return [ Float(param), self.to_f ]
    end
    super(param)
  end

  def self.induced_from(obj)
    if obj._isInteger
      obj
    elsif obj._isFloat
      obj.to_i
    else
      raise TypeError, "argument to induced_from neither Float nor Integer"
      nil
    end
  end

  def __fraised_to(arg)
    # handles coercion for _rubyRaisedTo:
    if arg._isInteger 
      raise TypeError , 'coercion error in ** '
    elsif arg._isNumeric
      c = arg.coerce(self)
      c[0] ** c[1]
    else
      raise TypeError, 'numeric coercion failed'
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

  primitive_nobridge '**', '_rubyRaisedTo:'
  primitive_nobridge '~', 'bitInvert'
  primitive_nobridge '&', '_rubyBitAnd:'
  primitive_nobridge '|', '_rubyBitOr:'
  primitive_nobridge '__prim_xor', '_rubyBitXor:'
  primitive_nobridge '<<', '_rubyShiftLeft:'

  def ^(arg)
    arg = Maglev::Type.coerce_to(arg, Integer, :to_int)
    if self < 0 && arg < 0
      if self == arg
        0
      else
        0 - ((0 - self).__prim_xor(arg))
      end
    else
      __prim_xor(arg)
    end
  end

  def >>(arg)
    unless arg._isFixnum
      arg = Maglev::Type.coerce_to(arg, Integer, :to_int)
      unless arg._isFixnum 
	if (self >= 0)
	  return 0
	else
	  return -1
	end
      end
    end
    self << ( 0 - arg )
  end

  # following handle primitive failures of  _rubyBitOr:, etc
  def __bit_and(arg)
    a = Maglev::Type.coerce_to(arg, Integer, :to_int) 
    self & a 
  end
 
  def __bit_or(arg)
    a = Maglev::Type.coerce_to(arg, Integer, :to_int) 
    self | a 
  end

  def __bit_xor(arg)
    a = Maglev::Type.coerce_to(arg, Integer, :to_int) 
    self.__prim_xor( a )
  end

  def __shift_left(arg)
    a = Maglev::Type.coerce_to(arg, Integer, :to_int) 
    unless a._isFixnum
      raise RangeError, 'argument must be a Fixnum'
    end
    self << a 
  end

  primitive '<',  '_rubyLt:'
  primitive '<=', '_rubyLteq:'
  primitive '>' , '_rubyGt:'
  primitive '>=', '_rubyGteq:'
  primitive '==', '_rubyEqual:'

  def <=>(arg)
    if arg._isInteger
      if self < arg
        -1
      elsif self == arg
        0
      else
        1  
      end
    elsif arg._isFloat
      sf = Maglev::Type.coerce_to(self, Float, :to_f)
      sf <=> arg
    else
      super
    end
  end

  primitive_nobridge '__bit_at', 'bitAt:'  # argument is 1 based

  def [](arg)
    a = Maglev::Type.coerce_to(arg, Integer, :to_int)
    if (a < 0)
       0
    else
      if a._isFixnum
        self.__bit_at(a + 1)
      else
        self < 0 ? 1 : 0 
      end
    end 
  end

  def abs
    if self < 0
      - self
    else
      self
    end
  end

  def ceil
    self
  end

  def chr
      if self > 255 || self < 0
	  raise RangeError, "#{self} out of char range"
      end
      string = ' '
      string[0] = self
      string
  end

  def divmod(arg)
    if arg._isInteger
      q = self.__divide(arg)
      r = self - (q * arg)
      [ q, r ]
    elsif arg._isNumeric
      c = arg.coerce(self)
      c[0].divmod(c[1])
    else
      raise TypeError, 'numeric coercion failed'
      nil
    end
  end

  def div(arg)
    if arg._isFloat 
      if arg == 0.0
	raise FloatDomainError, 'argument to div is zero'
      end
      self.to_f.div(arg)
    else
      q = self.__divide(arg)
      q.to_int
    end
  end

  def downto(n, &block)
    unless block_given?
      # added for 1.8.7 
      unless n._isNumeric
        raise TypeError, 'argument to downto must be a Numeric'
      end 
      return NumericDownEnumerator.new(self, self, n, 1)
    end
    arr = [1]
    broke = false
    ea_res = arr.each { |ignore| # this each handles break from the argument block
      k = self
      while k >= n
        broke = true
	block.call(k)
        broke = false
	k -= 1
      end
    }
    if broke
      return ea_res  # the argument block did a break
    end
    self
  end

  primitive 'eql?', '_ruby_eqlQ:'

  def even? # added for 1.8.7
    (self & 1)._equal?(0)
  end

  primitive 'floor', 'floor'

  primitive 'hash'

  def next
    self + 1
  end

  def nonzero?
    if self == 0
      nil
    else
      self
    end
  end

  def odd? # added for 1.8.7
    (self & 1)._equal?(1)
  end

  def ord # added for 1.8.7
    self
  end

  def pred  # added for 1.8.7
    self - 1
  end

  def quo(param)
     (self.to_f ).__divide(param)
  end

  #  def remainder ; end #  inherited from numeric

  primitive 'round', 'rounded'  

  def succ
    self + 1
  end

  primitive 'size', 'size'

  def times(&block)
    unless block_given?
      return NumericEnumerator.new(self, 0, self - 1 , 1) # for 1.8.7
    end
    arr = [1]
    broke = false
    k = 0
    while k < self
      ea_res = arr.each { |ignore| # this each handles break from the argument block
        while k < self
          broke = true
	  block.call(k)
          broke = false
	  k += 1
        end
      }
      if broke
        if ea_res._equal?(arr)
          # ruby_next  bytecode broke out of arr.each {} , see svn/src/intrubynext.hc
          k += 1  # continue the iteration
        else 
          # the argument block did a ruby  break
          return ea_res  
        end
      end
    end
    self
  end


  primitive 'to_f', 'asFloat'
  primitive '__to_float', 'asFloat'
  primitive 'to_i', 'truncated'
  primitive 'to_int' , 'truncated'

  primitive '__to_s_base_show', 'printStringRadix:showRadix:'

  # primitive to_s  is the zero arg form 
  primitive 'to_s', 'asString'

  def to_s(base)
    unless base._isFixnum 
      raise TypeError, 'arg must be a Fixnum'
    end
    __to_s_base_show(base, false).downcase
  end

  primitive 'truncate' , 'truncated'

  def upto(n, &block)
    unless block_given?
      # added for 1.8.7
      unless n._isNumeric
        raise TypeError, 'argument to upto must be a Numeric'
      end
      return NumericEnumerator.new(self, self, n, 1)
    end
    arr = [1]
    broke = false
    ea_res = arr.each { |ignore| # this each handles break from the argument block
      k = self
      while k <= n
        broke = true
	block.call(k)
        broke = false
	k += 1
      end
    }
    if broke
      return ea_res  # the argument block did a break
    end
    self
  end


  def zero? 
    self == 0
  end

  # Were in String.rb
  def __split_string(string, limit)
        self.chr.__split_string(string, limit)
  end

  # primitives added to support BigDecimal implementation

  class_primitive_nobridge '__from_string', 'fromString:'
  class_primitive_nobridge '__from_string_radix', 'fromString:radix:'

  primitive_nobridge '__decimal_digits_length_approx', '_decimalDigitsLength:' 
    # argument is useApproximationBoolean , if true result may  be
    # slightly smaller than actual number of digits

  primitive_nobridge '__min', 'min:'  # Smalltalk coercion on arg
  primitive_nobridge '__max', 'max:'  # Smalltalk coercion on arg
  primitive_nobridge '__quo_rem', 'quoRem:into:' # Smalltalk coercion on arg

end
