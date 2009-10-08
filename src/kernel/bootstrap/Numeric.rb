#---------------------------------
#  Ruby Numeric is identically  Smalltalk Number

class Numeric

  def coerce(param, &block)
    if param._isNumeric && param.class.equal?(self.class)
      return [ param, self ]
    end
    unless param.equal?(nil) || param._isSymbol
      begin
        if param._isString
          p = Float(param)
        else
          p = param.to_f
        end
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

  def abs
    if self < 0
      - self
    else
      self
    end
  end

  def ceil
    f = Type.coerce_to(self, Float, :to_f)
    f.ceil
  end

  # Most internal uses of /  must use _divide() to avoid
  #   infinite recursion after  math.n redefines quo and / for Rational

  def div(arg)
    unless arg._isNumeric
      raise TypeError, 'arg to div is not a Numeric'
    end
    q = (self / arg).to_f
    q.floor
  end

  def quo(arg)
    unless arg._isNumeric
      raise TypeError, 'arg to quo is not a Numeric'
    end
    self / arg
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

  def dup
    raise TypeError , 'Numeric#dup not allowed'
  end

  def remainder(arg)
    unless arg._isNumeric
      raise TypeError, 'arg to remainder is not a Numeric'
    end
    mod = self % arg
    rec_neg = self < 0
    arg_neg = arg < 0
    if (rec_neg == arg_neg)
      mod
    else
      mod - arg
    end
  end

  # eql?  implemented in subclasses

  def floor 
    self.to_f.floor
  end
 
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

  def nonzero?  
    # reimplemented in subclasses
    if self.zero?    
      nil
    else
      self
    end
  end

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
    self % arg
  end

  def round 
    # reimplemented in subclasses
    self.to_f.round
  end

  def step(nend, inc, &blk) 
    if nend._isFloat or inc._isFloat
      s = Type.coerce_to(self, Float, :to_f)
      s.step(nend, inc, &blk)
    else
      n = self
      if inc == 0 
        raise ArgumentError, "increment is zero"
      end
      if block_given?
        if inc > 0
          until n > nend
            blk.call(n)
            n += inc
          end
        else
          until n < nend
            blk.call(n)
            n += inc
          end
        end
      else
        if inc > 0
          unless n > nend 
            raise LocalJumpError, 'no block given'
          end
        else
          unless n < nend
            raise LocalJumpError, 'no block given'
          end
        end
      end
    end
  end

  def step(nend, &blk)
    if nend._isFloat
      s = Type.coerce_to(self, Float, :to_f)
      s.step(nend, &blk)
    else
      n = self
      if block_given?
        until n > nend 
	  blk.call(n)
	  n += 1
        end
      elsif n < nend
        raise LocalJumpError, 'no block given'
      end
    end
  end

  primitive_nobridge 'inspect', 'printString'


  # to_int implemented in subclasses, but this default implementation is
  # here for library classes like Rational.
  def to_int
    self.to_i
  end

  def truncate
    self.to_f.truncate
  end

  def zero?
    self == 0
  end

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

