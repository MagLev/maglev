module Math
  PI = 3.14159265358979
  E  = 2.71828182845905

  def acos(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    r = f.acos
    if r.nan?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def acosh(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    r = f.acosh
    if r.nan?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def asin(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    r = f.asin
    if r.nan?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def asinh(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.asinh
  end

  def atan(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.atan
  end

  def atan2(x, y)
    fx = Type.coerce_to(x, Float, :_to_f_or_error )
    fy= Type.coerce_to(y, Float, :_to_f_or_error )
    r = fx._atan2(fy)
    if r.nan?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def atanh(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    r = f.atanh
    if r.nan? || r.infinite?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def cos(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.cos
  end

  def cosh(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.cosh
  end

  def erf(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.erf
  end

  def erfc(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.erfc
  end

  def exp(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.exp
  end

  def frexp(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.frexp
  end

  def hypot(x, y)
    fx = Type.coerce_to(x, Float, :_to_f_or_error )
    fy= Type.coerce_to(y, Float, :_to_f_or_error )
    fx._hypot(fy)
  end

  def ldexp(x, y)
   fx = Type.coerce_to(x, Float, :_to_f_or_error )
   iy = Type.coerce_to(y, Fixnum, :to_int)
   fx._ldexp(iy)
  end

  def log(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    r = f.log
    if r.nan?
      if f == 0.0
        raise Errno::ERANGE
      else
        raise Errno::EDOM
      end
      nil
    else
      r
    end
  end

  def log2(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    r = f.log2
    if r.nan?
      if f == 0.0
        raise Errno::ERANGE
      else
        raise Errno::EDOM
      end
      nil
    else
      r
    end
  end

  def log10(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    r = f.log10
    if r.nan?
      if f == 0.0
        raise Errno::ERANGE
      else
        raise Errno::EDOM
      end
      nil
    else
      r
    end
  end

  def modf(x)
    # result is an Array,   [ fractional_part, integral_part ]
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.modf
  end

  def sin(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.sin
  end

  def sinh(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.sinh
  end

  def sqrt(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.sqrt
  end

  def tan(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.tan
  end

  def tanh(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.tanh
  end

  def sqrt(x)
    f = Type.coerce_to(x, Float, :_to_f_or_error )
    f.sqrt
  end

  module_function \
    :acos,
    :acosh,
    :asin,
    :asinh,
    :atan,
    :atan2,
    :atanh,
    :cos,
    :cosh,
    :erf,
    :erfc,
    :exp,
    :frexp,
    :hypot,
    :ldexp,
    :log,
    :log10,
    :log2,
    :modf,
    :sin,
    :sinh,
    :sqrt,
    :tan,
    :tanh

end
Math._freeze_constants
