#---------------------------------
#  Ruby Numeric is identically  Smalltalk Number

class Numeric

  def coerce(param)
    raise TypeError, 'numeric coercion failed'
    [ nil, nil]
  end

  def <=>(arg)
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

  primitive 'abs', 'abs'

  def ceil
    f = Type.coerce_to(self, Float, :to_f)
    f.ceil
  end

  # "Internal" uses of /  must use _divide() to avoid
  #   infinite recursion after  math.n redefines quo and / for Rational

  def div(arg)
    q = self._divide(arg)
    q.to_int
  end

  def quo(arg)
    self._divide(arg)
  end

  def divmod(arg)
    unless arg._isNumeric
      raise TypeError, 'arg to divmod is not a Numeric'
    end
    a = Type.coerce_to(arg, Float, :to_f)
    if a == 0.0
      raise FloatDomainError ,'arg to divmod was zero'
    end
    q = (self._divide(a)).floor
    r = self - (q * a)
    [ q, r ]
  end

  def remainder(arg)
    unless arg._isNumeric
      raise TypeError, 'arg to remainder is not a Numeric'
    end
    mod = self.modulo(arg)
    rec_neg = self < 0
    arg_neg = arg < 0
    if (rec_neg == arg_neg)
      mod
    else
      mod - arg
    end
  end

  # eql?  implemented in subclasses
  #  floor implemented in subclasses

  primitive 'hash'

  primitive 'integer?', '_isInteger'

  #  nonzero?  implemented in subclasses
  #  quo   implemented in subclasses

  # _max and _min allow
  #    a._max(b)
  #  which is much cheaper than the typical Ruby style
  #    [a,b].max
  #
  def _max(arg)
    unless arg._isNumeric
      raise TypeError, 'arg to _max is not a Numeric'
    end
    return self < arg ? arg : self
  end

  def _min(arg)
    unless arg._isNumeric
      raise TypeError, 'arg to _min is not a Numeric'
    end
    return self < arg ? self : arg
  end


  def modulo(arg)
    # reimplemented in subclasses
    (self.divmod(arg))[1]
  end

  # round implemented in subclasses

  primitive 'step', '_rubyTo:by:do:'

  primitive_nobridge 'inspect', 'printString'


  # to_int implemented in subclasses, but this default implementation is
  # here for library classes like Rational.
  def to_int
    self.to_i
  end

  # truncated implemented in subclasses
  # zero?  implemented in subclasses

  def +@
    self
  end

  def -@
    0 - self
  end

  def taint
    self # no-op
  end

  def untaint
    self # no-op
  end

  def tainted?
    false # lie...
  end

  def frozen?
    false # lie...
  end

  def freeze
    self # no-op
  end
end

