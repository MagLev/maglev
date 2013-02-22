class BigDecimal
  # constants which must be dynamic

  # modifications to these constants will be persistent if done within
  #  Maglev.persistent  block

  DEFAULT_prec = 0  

  ROUNDING_mode = ROUND_HALF_UP 
  RAISE_on_NaN = false
  RAISE_on_INF = false
  RAISE_on_UNDERF  = false
  RAISE_on_ZERODIV = false 

  # RAISE_on_OVERF = true , frozen, defined in bootstrap/bigdecimal1.rb
  # a FloatDomainError will also be raised whenever a Bignum value for 'digits'
  #  would exceed 130176 bits , due to the limitations of the Bignum implementation.


  def self.__mode(sym)
    # gets the specified mode
    if sym._equal?( :ROUND_MODE )
      return ROUNDING_mode
    elsif sym._equal?( :EXCEPTION_NaN ) 
      return RAISE_on_NaN
    elsif sym._equal?( :EXCEPTION_INFINITY )
      return RAISE_on_INF 
    elsif sym._equal?( :EXCEPTION_UNDERFLOW )
      return RAISE_on_UNDERF 
    elsif sym._equal?( :EXCEPTION_OVERFLOW )
      return RAISE_on_OVERF
    elsif sym._equal?( :EXCEPTION_ZERODIVIDE )
      return RAISE_on_ZERODIV
    else
      raise ArgumentError, 'unrecognized first arg to BigDecimal::mode, #{sym}'
    end
  end

  def self.__set_mode(sym, v)
    # sets the specified mode modification will be transient or persistent
    # per the current state of  Maglev.persistent?
    if sym._equal?( :ROUND_MODE )
      v = Maglev::Type.coerce_to(v, Fixnum, :to_int)
      if v < ROUND_DOWN or v > ROUND_UP
        raise ArgumentError, 'rounding mode must be one of -1,0,1'
      end
      self.const_set( :ROUNDING_mode , v)
    else
      unless v._equal?(true) or v._equal?(false)
        raise ArgumentError, 'second arg to mode must be true or false'
      end
      if sym._equal?( :EXCEPTION_NaN )
        self.const_set( :RAISE_on_NaN , v)
      elsif sym._equal?( :EXCEPTION_INFINITY )
        self.const_set( :RAISE_on_INF , v)
      elsif sym._equal?( :EXCEPTION_UNDERFLOW )
        self.const_set( :RAISE_on_UNDERF , v)
      elsif sym._equal?( :EXCEPTION_OVERFLOW )
        # value ignored, we always throw error
      elsif sym._equal?( :EXCEPTION_ZERODIVIDE )
        self.const_set( :RAISE_on_ZERODIV , v)
      else
        raise ArgumentError, 'unrecognized first arg to BigDecimal::mode, #{sym}'
      end
    end
  end

  def self.limit(val=nil)
    old_limit = DEFAULT_prec
    if val._not_equal?(nil)
      val = Maglev::Type.coerce_to( val, Fixnum, :to_int)
      raise TypeError , 'BigDecimal.limit, precision arg must be >= 0'   if val < 0
      self.const_set(:DEFAULT_prec, val )
    end
    return old_limit
  end

end
