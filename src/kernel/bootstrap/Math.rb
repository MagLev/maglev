module Math
  PI = 3.14159265358979
  E  = 2.71828182845905

  def acos(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.acos
  end

  def acosh(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.acosh
  end

  def asin(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.asin
  end

  def asinh(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.asinh
  end 

  def atan(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.atan
  end

  def atan2(x, y)
    fx = Type.coerce_to(x, Float, :to_f )
    fy= Type.coerce_to(y, Float, :to_f )
    fx._atan2(fy)
  end

  def atanh(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.atanh
  end 

  def cos(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.cos
  end

  def cosh(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.cosh
  end

  def erf(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.erf
  end

  def erfc(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.erfc
  end

  def exp(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.exp
  end

  def frexp(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.frexp
  end

  def hypot(x, y)
    fx = Type.coerce_to(x, Float, :to_f )
    fy= Type.coerce_to(y, Float, :to_f )
    fx._hypot(fy)
  end

  def ldexp(x, y)
   fx = Type.coerce_to(x, Float, :to_f )
   iy = Type.coerce_to(y, Fixnum, :to_int)
   fx._ldexp(iy)
  end

  def log(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.log
  end

  def log2(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.log2
  end

  def log10(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.log10
  end

  def modf
    # result is an Array,   [ fractional_part, integral_part ]
    f = Type.coerce_to(x, Float, :to_f )
    f.modf
  end

  def sin(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.sin
  end

  def sinh(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.sinh
  end

  def sqrt(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.sqrt
  end

  def tan(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.tan
  end

  def tanh(x)
    f = Type.coerce_to(x, Float, :to_f )
    f.tanh
  end

  def sqrt(x)
    f = Type.coerce_to(x, Float, :to_f )
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
