#---------------------------------
#  Ruby Numeric is identically  Smalltalk Number

class Numeric

  def coerce(param, &block)
    if param._isNumeric && param.class.equal?(self.class)
      return [ param, self ]
    end
    if ! param.equal?(nil) && ! param._isSymbol
      begin
        p = param.to_f
        s = self.to_f
        if p._isFloat && s._isFloat && ! p.nan? && ! s.nan?
          return [p, s]
        end
      rescue
        # continue execution
      end
    end
    raise TypeError, 'numeric coercion failed'
    [ nil, nil]
  end

  def <=>(arg)
    # Must be reimplemented in Float and Integer
    if arg._isNumeric
      if self == arg
        0
      else
        nil
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
 
  primitive '_to_float', 'asFloat'

  def hash
    h = 0
    begin
      # don't use to_f here; causes infinite recursion with mspec
      h = self._to_float.hash
    rescue
      # continue execution
    end
    h
  end

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

  def step(nend, &blk)
    n = Type.coerce_to(self, Float, :to_f)
    nend = Type.coerce_to(nend, Float, :to_f)
    if block_given?
      while n <= nend
	blk.call(n)
	n += 1.0
      end
    end
  end

  def step(nend, inc, &blk) 
    n = Type.coerce_to(self, Float, :to_f)
    nend = Type.coerce_to(nend, Float, :to_f)
    inc = Type.coerce_to(inc, Float, :to_f)
    if block_given?
      while n <= nend
	blk.call(n)
	n += inc
      end
    end
  end

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

  def _to_f_or_error
    self.to_f
  end
end

