module Math
  PI = 3.14159265358979
  E  = 2.71828182845905

  def acos(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    r = f.acos
    if r.nan?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def acosh(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    r = f.acosh
    if r.nan?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def asin(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    r = f.asin
    if r.nan?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def asinh(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.asinh
  end

  def atan(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.atan
  end

  def atan2(x, y)
    fx = Maglev::Type.coerce_to(x, Float, :to_f )
    fy= Maglev::Type.coerce_to(y, Float, :to_f )
    r = fx.__atan2(fy)
    if r.nan?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def atanh(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    r = f.atanh
    if r.nan? || r.infinite?
      raise Errno::EDOM
      nil
    else
      r
    end
  end

  def cos(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.cos
  end

  def cosh(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.cosh
  end

  def erf(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.erf
  end

  def erfc(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.erfc
  end

  def exp(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.exp
  end

  def frexp(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.frexp
  end

  def hypot(x, y)
    fx = Maglev::Type.coerce_to(x, Float, :to_f )
    fy= Maglev::Type.coerce_to(y, Float, :to_f )
    fx.__hypot(fy)
  end

  def ldexp(x, y)
   fx = Maglev::Type.coerce_to(x, Float, :to_f )
   iy = Maglev::Type.coerce_to(y, Fixnum, :to_int)
   fx.__ldexp(iy)
  end

  def log(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
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
    f = Maglev::Type.coerce_to(x, Float, :to_f )
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
    f = Maglev::Type.coerce_to(x, Float, :to_f )
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
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.modf
  end

  def sin(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.sin
  end

  def sinh(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.sinh
  end

  def sqrt(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.sqrt
  end

  def tan(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.tan
  end

  def tanh(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
    f.tanh
  end

  def sqrt(x)
    f = Maglev::Type.coerce_to(x, Float, :to_f )
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
Math.__freeze_constants
