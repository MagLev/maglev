# file bigdecimal.rb , comes after bigdecimal1.rb during bootstrap

def BigDecimal(string, _precs=0)
  BigDecimal.new(string, _precs)
end

class BigDecimal
  # most constants defined in bigdecimal1.rb
  # for internal structure see comments under Accessors below

  #################
  # Class methods #
  #################

  def self.double_fig
    16  # number of significant digits in a Float (8 byte IEEE float)
  end

  # def self.mode ; end # in post_prims/bigdecimal.rb
  # def self.limit ; end # in post_prims/bigdecimal.rb

  def self.induced_from(obj)
    if obj._isInteger
      self.__from_integer(obj)
    elsif obj._kind_of?(BigDecimal)
      obj
    else
      raise TypeError, "failed to convert #{obj.class} into BigDecimal"
    end
  end

  def self.ver
    VERSION
  end

  def self.mode(sym, value=MaglevUndefined)
    # sets the specified mode. modification will be transient or persistent
    # per the current state of  Maglev.persistent?
    if value._equal?(MaglevUndefined)
      self.__mode(sym)
    else
      self.__set_mode(sym, value) # implem in post_prims/bigdecimal.rb
    end
  end

  def self.mode(sym)
    # gets the specified mode
    self.__mode(sym)  # implem in post_prims/bigdecimal.rb
  end


  ###############################
  # private Accessors
  ###############################

  def __kind
    @special  # a Fixnum, 0 for finite, 1 for infinite, 2 for nan
  end

  def __digits
    @digits  # a Fixnum or Bignum
  end

  def __exp
    @exp # a Fixnum , the power of 10 by which to multiply @digits
         # to get the actual value.
         # A value of 0 means that @digits is the integer part
         #   of the BigDecimal and the fractional part is zero.
  end

  def __sign
    @sign  # a Fixnum -1 or 1
  end

  def __add_to_exp(v)
    @exp = @exp + v
  end

  def __precs
    @precs # number of digits of precision , always > 0
  end

  ###############################
  # public Accessors
  ###############################

  # Returns the exponent as a Fixnum (or 0 if out of range), such that the absolute value
  #  of the base is between 0 and 1.  This is not the power function.
  # call-seq:
  #   BigDecimal("0.125e3").exponent => 3
  #   BigDecimal("3000").exponent => 4
  #
  def exponent
    e = @exp
    digs = @digits
    unless digs == 0
      e += digs.__decimal_digits_length_approx(false)
    end
    e
  end

  def sign
    kind = @special
    if kind._equal?(0) # finite
      if self.zero?
        @sign._equal?(1) ? SIGN_POSITIVE_ZERO : SIGN_NEGATIVE_ZERO
      else
        @sign._equal?(1) ? SIGN_POSITIVE_FINITE : SIGN_NEGATIVE_FINITE
      end
    elsif kind._equal?(1) # infinite
       @sign._equal?(1) ? SIGN_POSITIVE_INFINITE : SIGN_NEGATIVE_INFINITE
    else # nan
      SIGN_NaN
    end
  end


  # As for Float.finite? .
  # call-seq:
  #   BigDecimal.new("Infinity").finite?  => false
  #   BigDecimal.new("NaN").finite?  => true
  def finite?
    @special._equal?(0)
  end

  def infinite?
    if @special._not_equal?(1)
      return nil # finite or  nan
    else
      return @sign  # returns 1 or -1
    end
  end

  # As for Float.nan? .
  # call-seq:
  #   BigDecimal.new("NaN").nan?  => true
  #   BigDecimal.new("123").nan?  => false
  def nan?
    @special._equal?(2)
  end

  # True if positive or negative zero; false otherwise.
  # call-seq:
  #   BigDecimal.new("0").zero?   =>true
  #   BigDecimal.new("-0").zero?  =>true
  def zero?
    #  @digits.to_i == 0 and self.finite?
    @digits == 0 && @special._equal?(0)
  end

  def precs
    if @special._equal?(0)
      sigfigs = @digits.__decimal_digits_length_approx(false)
      p = @precs
      if p._equal?(0)
        p = sigfigs + 10
      end
    else
      sigfigs = 0  # NaN or Infinity
      p = 1
    end
    [sigfigs, p]
  end

  ###############################
  # Constructors and initializers
  ###############################

  def self.__nan
    if RAISE_on_NaN
      raise FloatDomainError , 'result would be a NaN'
    end
    res = self.allocate
    res.__init_nan
  end

  def __init_nan
    @special = 2
    @precs = UNLIM_PRECISION
    @sign = 1
    @digits = 0
    # @num_digits = 1
    @exp = 0
    self
  end

  def self.__infinity(sign)
    if RAISE_on_INF
      raise FloatDomainError , 'result would be infinite'
    end
    res = self.allocate
    res.__init_infinity(sign)
  end

  def __init_infinity(sign_arg)
    @special = 1
    @sign = sign_arg
    @precs = UNLIM_PRECISION
    @digits = 0
    # @num_digits = 1
    @exp = 0
    self
  end

  def __init_zero(sign_arg)
    @special = 0
    @sign = sign_arg
    @precs = UNLIM_PRECISION
    @digits = 0
    # @num_digits = 1
    @exp = 0
    self
  end

  def self.__zero(sign)
    res = self.allocate
    res.__init_zero(sign)
  end

  def self.__from_integer(val)
    res = self.allocate
    res.__init_from_integer(val)
  end

  def __init_from_integer(val)
    @special = 0
    if val < 0
      @sign = -1
      val = 0 - val
    else
      @sign = 1
    end
    arr = __reduce_trailing_zeros(val)
    digs = arr[0]
    exp =  arr[1]  # number of trailing zeros removed
    unless exp._isFixnum
      raise FloatDomainError, 'exponent of a BigDecimal exceeds Fixnum range'
    end
    @exp = exp
    @digits = digs
    @precs = UNLIM_PRECISION
    self
  end

  def self.__from_float(val)
    res = self.allocate
    res.__init_from_float(val)
  end

  def __init_from_float(flt)
    if flt.finite?
      str = flt.__as_string  # Smalltalk print format sd.ddddEsee
      len = str.length
      exp_idx = len
      exp_done = false
      exp = 0
      ch = nil
      exp_mult = 1
      until exp_done
        exp_idx -= 1
        ch = str[exp_idx]
        if ch < ?0  # 'E' or exponent sign, either '+' or '-'
          if ch._equal?( ?- )
            exp = exp * -1
            exp_idx -= 1 # now at 'E'
          elsif  ch._equal?( ?+ )
            exp_idx -= 1 # now at 'E'
          end
          exp_done = true
        else
          exp = exp + (ch - ?0 ) * exp_mult
        end
        exp_mult = exp_mult * 10
      end
      idx = 0
      ch = str[idx]
      sign = 1
      if ch._equal?( ?- )
        sign = -1
        idx += 1 # skip leading -
        ch = str[idx]
      end
      mant_str = ' '
      mant_str[0] = ch
      idx += 2 # skip the two chars 'd.'
      mant_str << str[idx , exp_idx - idx]
      digs = Integer.__from_string_radix(mant_str, 10)
      @digits = digs
      exp += 1 #  flt._as_string produced  d.dddEee , convert to 0.ddddEee for BigDecimal
      exp -= mant_str.length  #  so @digits is an Integer
      @exp = exp
      @sign = sign
      @special = 0
      @precs = 16  # 16 is the limit of 8 byte IEEE float
    elsif flt.infinite?
      self.__init_infinity( flt > 0.0 ? 1 : -1)
    else
      self.__init_nan
    end
    self
  end

  def __init_normal( res_sign, res_digits, res_exp)
    unless res_exp._isFixnum
      raise FloatDomainError, 'exponent of a BigDecimal exceeds Fixnum range'
    end
    @special = 0
    if res_digits._equal?(0)
      res_sign = 1  # canonicalize zero results as positive zero
      res_exp = 0
    end
    @sign = res_sign
    @digits = res_digits
    @exp = res_exp
    self
  end

  def __set_precision( p )
    # a separate method from __init_normal, since Maglev has a max of 3 non-array
    # args to a method, without incurring bridge method or argument wrapping overhead.
    @precs = p
    self
  end

  # call-seq:
  #   BigDecimal("3.14159")   => big_decimal
  #   BigDecimal("3.14159", 10)   => big_decimal
  def initialize(_val, _precs=0)
    # MRI appears to  ignore the precision argument
    v = _val.strip
    first_ch = v[0]
    sgn = 1
    if first_ch._equal?( ?+ )
      first_ch = v[1]
    elsif first_ch._equal?( ?- )
      first_ch = v[1]
      sgn = -1
    end
    if first_ch._equal?( ?N ) && v == "NaN"
      self.__init_nan
      return
    elsif first_ch._equal?( ?I ) && v =~ /^[-+]?Infinity$/
      self.__init_infinity(sgn)
      return
    end
    @sign = sgn
    @special = 0
    v = v.__delete_underscore
    m = /^\s*(([-+]?)(\d*)(?:\.(\d*))?(?:[EeDd]([-+]?\d+))?).*$/.match(v)
    mant = 0
    expon = 0
    ndigits = 0
    if m._not_equal?(nil) # [
      @sign = sgn
      i_cls = Integer
      frac_str = m[4]
      if frac_str._equal?(nil)
        frac = 0
        nd_frac = 0
      else
        frlen = frac_str.length
        nd_frac = frlen
        if frlen._not_equal?(0)
          # strip trailing zeros from fraction
          fend_idx = frlen - 1
          while fend_idx > 0 && frac_str[fend_idx]._equal?( ?0 )
            fend_idx -= 1
          end
          frlen = fend_idx + 1
        end
        frac_prefix_str = frac_str[0, frlen] # without trailing zeros
        frac = i_cls.__from_string_radix( frac_prefix_str , 10)
      end
      int_str = m[3]
      nd_int = int_str.length
      if nd_int._equal?(0)
        int = 0
      elsif int_str[0]._equal?( ?0 )
        int = i_cls.__from_string_radix( int_str, 10 ) # leading zero digit
      else
        j = nd_int - 1
        if frac_str._equal?(nil)
          # strip trailing zeros off integer to reduce chance of integer overflow
          while int_str[j]._equal?( ?0 ) and j > 0
            expon += 1
            j -= 1
          end
        end
        int = i_cls.__from_string_radix( int_str[0, j+1], 10 )
      end
      exp_str =  m[5]
      expon +=  exp_str._equal?(nil) ? 0 : i_cls.__from_string_radix(  exp_str, 10 )
      if int == 0 && frac != 0
        expon -= frlen # adjust for decimal point at rhs of internal digits
        # adjust precision for number of leading zeros in fraction
        fidx = 0
        while fidx < frlen && frac_str[fidx]._equal?( ?0 )
          fidx += 1
          nd_frac -= 1
        end
      elsif frac_prefix_str._not_equal?(nil)
        # multiply int by 10**frac_prefix_str.length
        count = frac_prefix_str.length
        int = __multiply_by_tenpower(int, count)
        expon -= count
      end
      mant = int + frac
    end # ]
    # MRI appears to ignore precision arg to new  and add about 17 ...
    if nd_frac._equal?(0)
      @precs = UNLIM_PRECISION
    else
      @precs = nd_frac + nd_int + 17
    end
    @digits = mant
    unless expon._isFixnum
      raise FloatDomainError, 'exponent of a BigDecimal exceeds Fixnum range'
    end
    @exp = expon
  end

  ###############
  # Conversions #
  ###############

  def to_f
    kind = @special
    sign = @sign
    if kind._not_equal?(0)
      if kind._equal?(1)
        if sign._equal?(1)
          return +1.0/0.0
        else
          return -1.0/0.0
        end
      end
      return 0.0/0.0 # NaN
    end
    mant = @digits # an Integer
    expon = 10.0 ** @exp
    f = mant.to_f
    if sign._equal?( -1 )
      f = f * -1.0
    end
    f = f * expon
    f
  end

  def to_i
    if @special._not_equal?(0)
      return nil # NaN or Infinity
    end
    exp = @exp
    val = @digits
    if exp._not_equal?(0)
      if exp > 0
        val = __multiply_by_tenpower(val, exp)
      else
        val = __divide_by_tenpower(val, 0 - exp)
      end
    end
    if @sign._not_equal?(1)
      val = 0 - val
    end
    val
  end

  def to_s(arg=nil)   # [
    # parse the argument for format specs
    positive = ''
    format =  :eng
    spacing = 0
    if arg._not_equal?(nil)
      if arg._isFixnum
        spacing = arg > 0 ? arg : 0
      else
        unless arg._isString
          raise TypeError, 'BigDecimal#to_s, expected a String or Fixnum argument'
        end
        if arg.length._not_equal?(0)
          if arg.index( ?+ , 0)._not_equal?(nil)
            positive = '+'
          elsif arg.index( ?\s  , 0)._not_equal?(nil)
            positive = ' '
          end
          if arg.index( ?F , 0)._not_equal?(nil)
            format = :float
          end
          spacing = arg.to_i
        end
      end
    end

    kind = @special
    if kind._equal?(2)
      return 'NaN'
    end

    if @sign._equal?(1)
      str = positive
    else
      str = '-'
    end

    if kind._equal?(0) # finite
      value = @digits.to_s
      nd = value.length
      expon = @exp
      s_expon = nd + expon # convert to expon for 0.ddddEee
      if format._equal?( :float )
        # get the decimal point in place
        if s_expon >= nd
          value << ('0' * (s_expon - nd))
          value << '.0' # DECIMAL_POINT.0
        elsif s_expon > 0
          vstr = value[0, s_expon]
          vstr << ?. # DECIMAL_POINT
          vstr << value[s_expon..-1]
          value = vstr
        elsif s_expon <= 0
          vstr = '0.'  # 0.DECIMAL_POINT
          vstr << ('0' * (0 - s_expon))
          vstr <<  value
          value = vstr
        end
      elsif format._equal?( :eng )
        value = '0.' + value  #  0.DECIMAL_POINT +
        value << ?E # EXP
        value <<  s_expon.to_s
      end

      if spacing._not_equal?( 0 )
        radix = '.' # DECIMAL_POINT
        m = /^(\d*)(?:(#{radix})(\d*)(.*))?$/.match(value)
        # left, myradix, right, extra = m[1, 4].collect{|s| s.to_s}
        marr = m[1, 4]
        left = marr[0].to_s
        myradix = marr[1].to_s
        right = marr[2] .to_s
        extra = marr[3].to_s

        right_frags = []
        0.step(right.length, spacing) do |n|
          right_frags.push( right[n, spacing])
        end

        left_frags = []
        tfel = left.reverse
        0.step(left.length, spacing) do |n|
          left_frags.unshift( tfel[n, spacing].reverse)
        end

        right = right_frags.join(' ').strip
        left = left_frags.join(' ').strip
        value = left.to_s
        value << myradix.to_s
        value << right.to_s
        value << extra.to_s
      end
      str << value
    elsif kind._equal?(1)
      str << 'Infinity'
    end
    return str
  end # ]

  def inspect
    str = '#<BigDecimal:'
    parr = self.precs
    str << [nil, "'#{self.to_s}'", "#{parr[0]}(#{parr[1]})"].join(',')
    str << '>'
    return str
  end

  #########################
  # Arithmetic support #
  #########################

  def coerce(other)
    if other._isInteger
      [ BigDecimal.__from_integer(other), self ]
    elsif other._isFloat
      [ BigDecimal.__from_float(other), self]
    elsif other._kind_of?(BigDecimal)
      [other, self]
    elsif other._isNumeric
      [BigDecimal(other.to_s), self]
    else
      raise TypeError, 'coercion to BigDecimal failed'
    end
  end

  def __reduce_precis_by(an_integer, reduction)
    # reduction is a negative number
    #   reduction is the number of l.s. digits to delete
    # returns an_integer divided by the requisite power of 10  .
    val = an_integer
    count = reduction
    if count > 9
      p = count - 9
      val = val.__divide( 10.__raised_to( p ))
      count -= p                # count now 9
    end
    if count > 0
      divisor = 10.__raised_to( count )
      arr = val.__quo_rem( divisor , [ nil, nil ] )
      val = arr[0]
      rem = arr[1]
      if rem._not_equal?(0)
        mode = ROUNDING_mode  # a dynamic constant from post_prims/bigdecimal.rb
        if mode._equal?(ROUND_HALF_UP)
          if rem > (divisor.__divide(2) )
            val += 1
          end
        elsif mode._equal?( ROUND_UP)
          val += 1
        else
          # ROUND_DOWN, add nothing
        end
      end
    end
    val
  end

  def __reduce_trailing_zeros(an_integer)
    # removes trailing decimal zero digits from an_integer
    # returns  [ reduced_integer, count ] ,
    #   where count tells how many trailing zeros were removed.
    val = an_integer
    count = 0
    qr_first = val.__quo_rem( 10,  [nil,nil] )
    if qr_first[0]._not_equal?(0) && qr_first[1]._equal?(0)
      #  (val > 10) && (val % 10) == 0) == true
      more_than_one_zero = false
      qr = [nil,nil]
      if val >= 1000_000_000
        val.__quo_rem( 1000_000_000 ,  qr )
        while (v_next = qr[0])._not_equal?(0) && qr[1]._equal?(0)
          # ((val >= 1000_000_000) && (val % 1000_000_000)==0)==true
          val = v_next
          count += 9
          more_than_one_zero = true
          val.__quo_rem( 1000_000_000, qr)
        end
      end
      val.__quo_rem( 1000, qr )
      while (v_next = qr[0])._not_equal?(0) && qr[1]._equal?(0)
        # ((val >= 10000) && (val % 1000)==0)==true
        val = v_next
        count += 3
        more_than_one_zero = true
        val.__quo_rem( 1000, qr )
      end
      if more_than_one_zero
        val.__quo_rem( 10 , qr )
      else
        qr = qr_first
      end
      while (v_next = qr[0])._not_equal?(0) && qr[1]._equal?(0)
        # ((val >= 10) && (val % 10)._equal?(0))==true
        val = v_next
        count += 1
        val.__quo_rem( 10 , qr )
      end
    end
    qr_first[0] = val # reuse qr_first as result array
    qr_first[1] = count
    qr_first
  end

  def __multiply_by_tenpower(an_integer, power)
    val = an_integer
    if power > 0
      p = 10.__raised_to( power)
      val = val * p
    else
      raise ArgumentError , '__multiply_by_tenpower, power <= 0'
    end
    val
  end

  def __divide_by_tenpower(an_integer, power)
    val = an_integer
    if power > 0
      p = 10.__raised_to( power )
      val = val.__divide(p)
    else
      raise ArgumentError , '__divide_by_tenpower, power <= 0'
    end
    val
  end

  def __negated
    my_kind = @special
    if my_kind._equal?(0)
      res = self.class.allocate
      res.__init_normal(  0 - @sign, @digits , @exp)
      res.__set_precision( @precs )
    elsif my_kind._equal?(1)
      res = self.class.allocate
      res.__init_infinity( 0 - @sign )
    else
      res = self # nan
    end
    res
  end

  #########################
  # Derived Arithmetic operations
  #########################

  # DEFAULT_prec is a dynamic constant, defined in  post_prims/bigdecimal.rb

  def +(other)
    unless other._kind_of?(BigDecimal)
      other = self.coerce(other)[0]
    end
    self.__add(other, DEFAULT_prec, other.__sign)
  end

  def sub(other, precs)
    precs = Maglev::Type.coerce_to( precs, Fixnum, :to_int)  # a required argument
    raise TypeError , 'precision must be >= 0'    if precs < 0
    unless other._kind_of?(BigDecimal)
      other = self.coerce(other)[0]
    end
    self.__add(other, precs, 0 - other.__sign  )
  end

  def -(other)
    unless other._kind_of?(BigDecimal)
      other = self.coerce(other)[0]
    end
    self.__add(other, DEFAULT_prec, 0 - other.__sign )
  end

  def *(other)
    self.mult(other, nil)
  end

  def quo(other, prec = nil)
    self.div(other, prec)
  end
  alias / quo

  def remainder(other)
    unless other._kind_of?(BigDecimal)
      other = self.coerce(other)[0]
    end
    mod = self.modulo(other)

    if (@sign * other.__sign < 0)
      return mod - other
    else
      return mod
    end
  end

  def modulo(other)
    self.divmod(other)[1]
  end
  alias % modulo

  #########################
  # Fundamental Arithmetic operations
  #########################


  def add(other, prec_arg)  # [
    prec_arg = Maglev::Type.coerce_to( prec_arg, Fixnum, :to_int) # a required arg
    raise TypeError , 'precision must be >= 0'    if prec_arg < 0
    unless other._kind_of?(BigDecimal)
      other = self.coerce(other)[0]
    end
    self.__add(other, prec_arg, other.__sign)
  end

  def __add(other, prec_arg, other_sign)
    my_kind = @special # 0 for normal, 1 for Infinity, 2 for NaN
    other_kind = other.__kind
    my_sign =  @sign
    unless (my_kind + other_kind)._equal?(0)
      # at least one is not finite
      if my_kind._equal?(2) or other_kind._equal?(2)
        return self.class.__nan  # at least one NaN
      elsif my_kind._equal?(1) and other_kind._equal?(1) and my_sign._not_equal?(other_sign)
        return self.class.__nan # infinity + -infinity
      elsif my_kind._equal?(1)
        return self.class.__infinity( my_sign )  #  infinity + x
      elsif other_kind._equal?(1)
        return self.class.__infinity( other_sign)  # x + infinity
      end
    end
    my_digs = @digits
    my_exp = @exp
    other_digs = other.__digits
    other_exp = other.__exp
    if prec_arg._equal?(0)
      if my_digs == 0 and other_sign == my_sign
        return other  # 0 + other
      elsif other_digs == 0 and other_sign == my_sign
        return self   # self + 0
      end
    end
    my_prec =  @precs
    other_prec = other.__precs
    if my_exp < other_exp
      delta_exp = other_exp - my_exp
      other_digs = self.__multiply_by_tenpower(other_digs, delta_exp)
      if other_prec < UNLIM_PRECISION
        other_prec += delta_exp
      end
      r_expon = my_exp
    elsif my_exp > other_exp
      delta_exp = my_exp - other_exp
      my_digs = self.__multiply_by_tenpower(my_digs, delta_exp)
      if my_prec < UNLIM_PRECISION
        my_prec += delta_exp
      end
      r_expon = other_exp
    else
      r_expon = my_exp
    end
    if my_sign._equal?(other_sign)
      r_digits = my_digs + other_digs
      r_sign = my_sign
    else
      if my_sign._equal?(-1)
        r_digits = other_digs - my_digs
      else
        r_digits = my_digs - other_digs
      end
      r_sign = 1
      if r_digits < 0
        r_sign = -1
        r_digits = 0 - r_digits
      end
    end
    arr = __reduce_trailing_zeros(r_digits)
    r_digits = arr[0]
    r_expon += arr[1]

                # adjust precision
    p = my_prec.__max(other_prec)  # max because by default no loss of precision on add
    if prec_arg._not_equal?(0)
      p = prec_arg.__min( p )
    end
    nd = r_digits.__decimal_digits_length_approx(false)
    rd = nd - p
    if rd > 0  #  rd is number of digits to omit
      r_digits = __reduce_precis_by(r_digits, rd)
      r_expon += rd
    end

    res = self.class.allocate
    res.__init_normal( r_sign, r_digits, r_expon)
    res.__set_precision(p)
  end # ]

  def mult(other, prec_arg = nil) # [
    if prec_arg._equal?(nil)
      prec_arg = DEFAULT_prec
    else
      prec_arg = Maglev::Type.coerce_to( prec_arg, Fixnum, :to_int)
      raise TypeError , 'precision must be >= 0'   if prec_arg < 0
    end
    unless other._kind_of?(BigDecimal)
      other = self.coerce(other)[0]
    end
    my_kind = @special # 0 for normal, 1 for Infinity, 2 for NaN
    other_kind = other.__kind
    my_sign =  @sign
    other_sign = other.__sign
    r_sign =  my_sign * other_sign
    unless (my_kind + other_kind)._equal?(0)
      my_zero = self.zero?
      other_zero = other.zero?
      if my_kind._equal?(2) or other_kind._equal?(2)
        return self.class.__nan # at least one is Nan
      elsif (my_kind._equal?(1) and other_zero) or (my_zero and other_kind._equal?(1) )
        return self.class.__nan   # a zero * an Infinity --> nan
      elsif my_kind._equal?(1)   # self is an Infinity
        if ( r_sign < 0) == my_sign < 0
          return self
        else
          return self.__negated
        end
      elsif other_kind._equal?(1)  # other is an Infinity
        if ( r_sign < 0) == my_sign < 0
          return other
        else
          return other.__negated
        end
      else
        raise 'logic error BigDecimal#mult non-finite'
      end
    end
    my_digs = @digits
    other_digs = other.__digits
    if my_digs == 0 or other_digs == 0
      res = self.class.__zero( r_sign )
    end
    my_exp = @exp
    other_exp = other.__exp

    r_digs = my_digs * other_digs
    r_expon =  my_exp + other_exp

    arr = __reduce_trailing_zeros( r_digs )
    r_digits = arr[0]
    r_expon += arr[1]

    my_prec =  @precs
    other_prec = other.__precs
                # adjust precision
    p = my_prec.__min(other_prec)
    if prec_arg._not_equal?(0)
      p = prec_arg.__min( p )
    end
    nd = r_digits.__decimal_digits_length_approx(false)
    rd = nd - p
    if rd > 0  #  rd is number of digits to omit
      r_digits = __reduce_precis_by(r_digits, rd)
      r_expon += rd
    end
    res = self.class.allocate
    res.__init_normal( r_sign, r_digits, r_expon)
    res.__set_precision(p)
  end # ]

  def div(other, prec_arg = nil) # [
    if prec_arg._equal?(nil)
      prec_arg = DEFAULT_prec
    else
      prec_arg = Maglev::Type.coerce_to( prec_arg, Fixnum, :to_int)
      raise TypeError , 'precision must be >= 0'   if prec_arg < 0
    end
    unless other._kind_of?(BigDecimal)
      other = self.coerce(other)[0]
    end
    my_kind = @special # 0 for normal, 1 for Infinity, 2 for NaN
    other_kind = other.__kind
    my_sign =  @sign
    other_sign = other.__sign
    r_sign =  my_sign * other_sign
    unless (my_kind + other_kind)._equal?(0)
      # at least one is not finite
      if my_kind._equal?(2) or other_kind._equal?(2)
        return self.class.__nan  # at least one NaN
      elsif other_kind._equal?(1)  # other is infinite
        if my_kind._equal?(1)
          return self.class.__nan  # infinity/infinity
        elsif prec_arg._equal?(0)
          return self.class.__nan  # finite/infinity and no precision specified
        else
          return self.class.__zero(1) # positive zero
        end
      elsif my_kind._equal?(1) # infinity / finite
        if prec_arg._equal?(0)
          return self.class.__nan # no precision specified
        else
          return self.class.__infinity( r_sign  )
        end
      end
    end
    my_digs = @digits
    my_exp = @exp
    other_digs = other.__digits
    other_exp = other.__exp
    if other_digs._equal?(0)
      if RAISE_on_ZERODIV
        raise FloatDomainError, 'divide by zero'
      end
      if my_digs == 0 or prec_arg._equal?(0)
        return self.class.__nan # 0 / 0 , or finite/0 with default precision
      else
        return self.class.__infinity( other_sign )  # finite non-zero / 0
      end
    end

    my_prec =  @precs
    other_prec = other.__precs
    my_nd = my_digs.__decimal_digits_length_approx(true)
    other_nd = other_digs.__decimal_digits_length_approx(true)
    delta_exp = prec_arg._equal?(0) ? 0 : prec_arg
    if my_prec < UNLIM_PRECISION
      delta_exp = delta_exp.__max(my_prec)
    end
    if other_prec < UNLIM_PRECISION
      delta_exp = delta_exp.__max(other_prec)
    end
    delta_nd = other_nd + 5
    if (other_nd > my_nd)
      delta_nd += (other_nd - my_nd)*2
    else
       delta_nd += 5
    end
    delta_exp = delta_nd.__max( delta_exp)
    if delta_exp > 0
      my_digs = __multiply_by_tenpower(my_digs, delta_exp)
    end
    r_digits = my_digs.__divide( other_digs )
    r_expon = my_exp - delta_exp - other_exp

                # adjust precision
    p = my_prec.__min(other_prec)
    if prec_arg._not_equal?(0)
      p = prec_arg.__min( p )
    end
    nd = r_digits.__decimal_digits_length_approx(false)
    rd = nd - p
    if rd > 0  #  rd is number of digits to omit
      r_digits = __reduce_precis_by(r_digits, rd)
      r_expon += rd
    end

    res = self.class.allocate
    res.__init_normal( r_sign, r_digits, r_expon)
    res.__set_precision(p)
  end # ]

  def divmod(other_arg) # [
    other = other_arg
    unless other._kind_of?(BigDecimal)
      other = self.coerce(other)[0]
    end
    my_kind = @special
    other_kind = other.__kind
    if my_kind._not_equal?(0) # self.infinite? or self.nan?
      cls = self.class
      return [ cls.__nan, cls.__nan ]
    end
    if other_kind._not_equal?(0) or other.zero?
      cls = self.class
      return [ cls.__nan, cls.__nan ]
    end

    first = (self.div(other, 0) ).floor(0)  # (first / other).floor
    prod = first.mult(other, 0)
    second = self.__add( prod, 0, 0 - prod.__sign )   #  self - (first * other)
    if other_arg._isFloat
      res = [ first.to_i , second.to_f ]
    else
      res = [ first , second ]
    end
    res
  end # ]

  def sqrt(prec_arg)   # [
    # precision arg is required, not optional
    prec_arg = Maglev::Type.coerce_to( prec_arg, Fixnum, :to_int)
    raise TypeError , 'precision must be >= 0'   if prec_arg < 0

    my_sign = @sign
    if my_sign._equal?(-1)
      unless self.zero?
        raise FloatDomainError , 'BigDecimal#sqrt, receiver is < 0 '
      end
    end
    if @special._equal?(2)
      raise FloatDomainError , 'BigDecimal#sqrt, receiver is NaN'
    end
    true_exp = self.exponent # the true exponent
    # avoid Infinity on conversion, Float handles E+-307 approx
    if true_exp > 300
      d_exp = true_exp - 300
      if (d_exp & 1) == 1
        d_exp += 1 # make it even
      end
      divisor = self.class.allocate
      divisor.__init_normal( 1, 1, d_exp )
      divisor.__set_precision(UNLIM_PRECISION)
      reduced_bd = self.div(divisor, 0)
      r_expon = d_exp.__divide(2)
    elsif true_exp < -300
      m_exp = 0 - (true_exp + 300)
      if (m_exp & 1) == 1
        m_exp += 1 # make it even
      end
      mu = self.class.allocate
      mu.__init_normal( 1, 1, m_exp )
      mu.__set_precision(UNLIM_PRECISION)
      reduced_bd = self.mult(mu, 0)
      r_expon = (0 - m_exp).__divide(2)
    else
      reduced_bd = self
      r_expon = 0
    end
    f = reduced_bd.to_f
    sq = f.sqrt   # use the Float math support
    sq_bd = self.class.__from_float(sq)
    if prec_arg < 14
      res = sq_bd
      if prec_arg > 0
        res = res.__add(self.class.__zero(1), prec_arg, 1) # to reduce precision
      end
      res.__add_to_exp( r_expon )
    else
      # Newtons method iteration to make result more precise
      # see http://en.wikipedia.org/wiki/Newton's_method
      margin = sq_bd.clone
      margin.__add_to_exp( 0 - prec_arg - 1) # margin = sq / (10**(prec_arg + 1))
      x = sq_bd
      x.__add_to_exp( r_expon )
      x.__set_precision(UNLIM_PRECISION)
      done = false
      my_sign_negated = 0 - my_sign
      until done
        delta = (x.mult(x, 0)).__add(self, 0, my_sign_negated ) # delta = (x * x) - self
        delta_sign = delta.__sign
        if delta_sign < 0
          diff = margin.__add(delta, 0, delta_sign )
        else
          diff = margin.__add(delta, 0, 0 - delta_sign ) # margin - sub
        end
        done = diff.__sign > 0 # done = margin > delta
        unless done
          # goal is x,  such that x*x == self
          # f(x) is   x**2 - self
          # first derivative f'(x) is   2*x
          # next_x = x - ( f(x) / f'(x))
          next_x = x - (((x * x) - self) / (2 * x))
          x = next_x
        end
      end
      res = x
    end
    res
  end # ]

  # Raises self to an integer power.
  def __power(other) # [
    other = Maglev::Type.coerce_to( other, Fixnum, :to_int)
    kind = @special
    if kind._not_equal?(0) # not finite
      return self.class.__nan
    elsif self.zero?
      if other > 0
        return self.class.__zero(1)
      elsif other == 0
        return self.class.__from_integer( 1 )
      else
        return self.class.__infinity(1)
      end
    end
    if other == 0
      one = self.class.__from_integer( 1 )
      if self == one
        return one
      end
    end
    if other < 0
      pos_pwr = self.__power( 0 - other )
      one = self.class.__from_integer( 1 )
      q = one.div( pos_pwr )
      return q
    end
    nd = @digits ** other
    if (@sign == -1)
      nsign = (other & 1) == 0 ? 1 : -1 #  even arg --> positive result
    else
      nsign = 1
    end
    nexp = @exp * other
    arr = __reduce_trailing_zeros(nd)
    nd = arr[0]
    nexp += arr[1]
    res = self.class.allocate
    res.__init_normal( nsign, nd, nexp)
    res.__set_precision(UNLIM_PRECISION)
  end # ]

  def power(other)
    res = self.__power(other)
    res.__reduce_to_precision(@precs)
  end

  alias ** power

  def __reduce_to_precision(p)
    if p != UNLIM_PRECISION
      if p < @precs
        r_digits = @digits
        nd = r_digits.__decimal_digits_length_approx(false)
        rd = nd - p
        if rd > 0  #  rd is number of digits to omit
          r_digits = __reduce_precis_by(r_digits, rd)
          r_expon = @exp + rd
          res = self.class.allocate
          res.__init_normal( @sign, r_digits, r_expon)
          res.__set_precision(p)
          return res
        end
      end
    end
    self
  end

  # Unary minus
  def -@
    self.__negated
  end

  def <=>(other) # [
    my_kind = @special
    if other._equal?(self)
      if my_kind._equal?(2)
        return nil # NaN's not comparable
      end
      return 0
    end
    unless other._isNumeric
      return nil
    end
    if !other._kind_of?(BigDecimal)
      return self <=> self.coerce(other)[0]
    end
    other_kind = other.__kind
    if (my_kind + other_kind)._not_equal?(0)
      # not both finite
      if my_kind._equal?(2) or other_kind._equal?(2)
        # at least one is nan
        return nil
      end
      if my_kind._equal?(1)
        if other_kind._equal?(1)
          # both infinite
          return @sign <=> other.__sign
        else
          return @sign # self infinite, other finite
        end
      else
        # self finite, other infinite
        return  0 - other.__sign
      end
    end
    res = (@sign <=> other.__sign)
    digs = @digits
    if res._not_equal?(0)
      if digs == 0 and other.__digits == 0
        return 0
      else
        return res
      end
    end
    res = (@exp <=> other.__exp)
    if res._equal?(0)
      return @digits <=> other.__digits
    end
    diff = self.__add(other , 0, 0 - other.__sign )  # self - other
    if diff.zero?
      return 0
    end
    return diff.__sign
  end # ]

  def eql?(other)
    if other._isNumeric
      if other._kind_of?(BigDecimal)
        return (self <=> other)._equal?(0)
      else
        oth = self.coerce(other)[0]
        return (self <=> oth)._equal?(0)
      end
    else
      return false
    end
  end
  alias === eql?

  def <(other)
    s = self <=> other
    if s._equal?(nil)
      if other._isNumeric
        return false  # at least one was NaN
      end
      raise ArgumentError, ' <=> returned a non-Numeric'
    end
    s < 0
  end

  def <=(other)
    s = self <=> other
    if s._equal?(nil)
      if other._isNumeric
        return false  # at least one was NaN
      end
      raise ArgumentError, ' <=> returned a non-Numeric'
    end
    s <= 0
  end

  def >(other)
    s = self <=> other
    if s._equal?(nil)
      if other._isNumeric
        return false  # at least one was NaN
      end
      raise ArgumentError, ' <=> returned a non-Numeric'
    end
    s > 0
  end

  def >=(other)
    s = self <=> other
    if s._equal?(nil)
      if other._isNumeric
        return false  # at least one was NaN
      end
      raise ArgumentError, ' <=> returned a non-Numeric'
    end
    s >= 0
  end

  def ==(other)
    s = self <=> other
    if s._equal?(nil)
      if other._isNumeric
        return false  # at least one was NaN
      end
      raise ArgumentError, ' <=> returned a non-Numeric'
    end
    s._equal?(0)
  end

  def between?(min, max)
    (min <= self) && (self <= max)
  end

  ####################
  # Other operations #
  ####################


  def abs
    if @sign._equal?(1)
      self
    else
      self.__negated
    end
  end

  def __truncate(n, delta_for_fraction) # [
    kind = @special
    if kind._not_equal?(0)
      return self  # Infinity or NaN
    end
    my_exp = @exp
    digs = @digits
    if n._not_equal?(0)
      if n > 0
        if my_exp < 0
          nd_frac = 0 - my_exp
          rd = nd_frac - n # rd is number of digits to omit
          if rd > 0
            digs = __reduce_precis_by(digs, rd)
            my_exp += rd
          end
        end
      else # n < 0 , zeroing digits to left of decimal
        rd = (0 - n) - my_exp
        if rd > 0
          digs = __reduce_precis_by(digs, rd)
          my_exp += rd
        end
      end
      res = self.class.allocate
      res.__init_normal( @sign , digs, my_exp)
      res.__set_precision(n)
    else  # n == 0
      int_val = digs
      if my_exp._not_equal?(0)
        if my_exp > 0
          int_val = __multiply_by_tenpower(digs, my_exp)
        else
          int_val = __divide_by_tenpower(digs, 0 - my_exp)
        end
      end
      my_sign = @sign
      if my_sign._not_equal?(1)
        int_val = 0 - int_val
      end
      if my_exp < 0 and delta_for_fraction != 0
        # has a fractional part
        pwr = 10.__raised_to( 0 - my_exp )
        frac = digs % pwr
        if frac > 0 && my_sign._equal?(delta_for_fraction)
          int_val += delta_for_fraction
        end
      end
      res = self.class.allocate
      res.__init_from_integer(int_val)
    end
  end # ]

  def ceil(n = 0)
    # if n == 0 , returns a BigDecimal with integer greater or equal to self
    # if n > 0,  returns copy of receiver
    #    with precision reduced to n digits to right of the decimal point
    # if n < 0, zeros (0-n) digits to left of decimal point
    n = Maglev::Type.coerce_to( n, Fixnum, :to_int)
    self.__truncate(n, 1)
  end

  def fix
    #  returns a BigDecimal with integer part of self
    my_exp = @exp
    if my_exp < 0
      int_val = self.to_i
      self.class.__from_integer(int_val)
    else
      self  # contains no fractional part
    end
  end

  def floor(n = 0)
    # if n == 0, returns a BigDecimal with integer value smaller or equal to self
    # if n > 0,  returns copy of receiver
    #    with precision reduced to n digits to right of the decimal point
    # if n < 0, zeros (0-n) digits to left of decimal point
    n = Maglev::Type.coerce_to( n, Fixnum, :to_int)
    self.__truncate(n, -1)
  end

  def frac
    # returns a BigDecimal the fractional part of self
    kind = @special
    if kind._not_equal?(0)
      return self  # Infinity or NaN
    end
    my_exp = @exp
    if my_exp < 0
      # has a fractional part
      pwr = 10.__raised_to( 0 - my_exp)
      frac = @digits % pwr
      if frac > 0
        res = self.class.allocate
        res.__init_normal(@sign, frac , my_exp)
        return res.__set_precision(UNLIM_PRECISION)
      end
    end
    return self.class.__zero(1)
  end

  def split
    # returns an Array, [ sign , digits_string, 10 , exponent ]
    #   sign is zero for NaN
    #   digits_string are the significant digits
    kind = @special
    if kind._not_equal?(0)
      if kind._equal?(1)
        return [ @sign, 'Infinity', 10, 0]
      else
        return [ 0, 'NaN', 10, 0]
      end
    end
    [ @sign , @digits.to_s , 10, self.exponent ]
  end

  def truncate(n = 0)
    # if n == 0, returns the integer part as a BigDecimal
    # if n > 0,  returns copy of receiver
    #    with precision reduced to n digits to right of the decimal point
    # if n < 0, zeros (0-n) digits to left of decimal point
    n = Maglev::Type.coerce_to( n, Fixnum, :to_int)
    self.__truncate(n, 0)
  end

  def self._load(data) # used by marshal
    raise TypeError, 'marshaled bignum format differ' unless data.start_with? "18:"
    begin
      self.new(data["18:".size..-1])
    rescue Exception
      raise TypeError, 'marshaled bignum format differ'
    end
  end

  def _dump(limit = nil)  # used by marshal
    "18:#{self.to_s}" # 18: is for compat with Ruby marshal
  end
end
