class Fixnum
  # Fixnum is identically  Smalltalk SmallInteger, 61 bit signed integers

  MAX =  1152921504606846975 # 0xfffffffffffffff
  MIN = -1152921504606846976

  def self.induced_from(obj)
    i = Maglev::Type.coerce_to(obj, Integer, :to_int)
    unless i._isFixnum
      raise RangeError, 'Object is out of range for a Fixnum' 
    end
    i
  end

  #  The 3 selectors  + - *   should be emitted as special sends and don't need
  #  methods installed. They will fall back to Bignum (to implementations in
  #  Smalltalk Integer) if the special send fails.

  # The selectors 
  #    + - * >= <= < &  
  #  are special sends and may not be reimplemented in Fixnum.

  primitive_nobridge '__isSpecial', 'isSpecial'

  primitive '<',  '_rubyLt:'
  primitive '<=', '_rubyLteq:'
  primitive '>' , '_rubyGt:'
  primitive '>=', '_rubyGteq:'
  primitive '==', '_rubyEqual:'

  primitive '===', '_rubyEqual:'  #  === same as == for Fixnum

  primitive 'between?', 'between:and:'

  #  /    # note division does not produce Fractions in Ruby
  #         until math.n is required, then may produce Rationals ...
  primitive_nobridge '/', '_rubyDivide:'
  primitive_nobridge '__divide', '_rubyDivide:'

  primitive_nobridge '__quo_rem', 'quoRem:into:' # Smalltalk coercion on arg, used by BigDecimal

  primitive_nobridge '%', '_rubyModulo:'
  primitive_nobridge 'modulo', '_rubyModulo:'

  primitive_nobridge '__raised_to' , '_rubyRaisedTo:'
  def **(arg)
    if arg._isInteger && arg >= 0
      return self.__raised_to(arg) 
    end
    if arg._isNumeric
      if (arg <=> 0) < 0
        r = self.to_f
        return r ** arg
      end
    end
    self.__raised_to(arg)
  end

  # unaries  +@  -@  eliminated during IR generation by compiler

  #  ~ inherited from Integer
  primitive_nobridge '&', '_rubyBitAnd:'
  primitive_nobridge '|', '_rubyBitOr:'
  primitive_nobridge '^', '_rubyBitXor:'
  primitive_nobridge '__prim_xor', '_rubyBitXor:'
  primitive_nobridge '<<', '_rubyShiftLeft:'
  # >> inherited from Integer

  def <=>(arg)
    # reimplemented to reduce use of polymorphic send caches
    if arg._isNumeric
      if self > arg
        1 
      elsif self == arg
        0 
      else
        -1 
      end
    else
      nil
    end
  end

  primitive_nobridge '__bit_at', 'bitAt:'  # argument is 1 based

  # abs inherited from Integer

  def clone  
    raise TypeError , 'cannot clone a Fixnum'
  end

  primitive 'id2name', '_ruby_id2name'

  # quo inherited from Integer
  primitive 'size', '_rubySize'
  primitive 'to_f', 'asFloat'

  # to_s inherited from Integer

  # ruby 1.9 has no to_sym method for Fixnums
  # primitive 'to_sym', '_rubyToSym'

  def zero?  
    self._equal?(0)
  end

  def nonzero?
    if self._equal?(0)
      nil
    else
      self
    end
  end

end
Fixnum.__freeze_constants
