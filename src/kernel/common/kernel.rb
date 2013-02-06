# All the code in this file comes from rubinius common/kernel.rb.  But, not
# all of the code in common/kernel.rb is in here...
module Kernel
  def Float(obj)
    return obj  if obj._isFloat
    if obj._isString
      if obj.__indexOfByte( 0 , 1 )._not_equal?(0)
        raise ArgumentError, 'null character in arg to Float()'
      end
      str = obj.lstrip  # remove leading white space
      str.rstrip! # remove trailing white space, in place
      sign = 1.0
      first_ch = str[0]
      if first_ch._equal?( ?+ )
        str.__remove_from_to(1,1)
      elsif first_ch._equal?( ?- )
        sign = -1.0
        str.__remove_from_to(1,1)
      end
      # see kernel/parser/lexer.rb for regexp's from which this code derived
      ok = (ra = str =~ /^[\d]+(.[\d]+)?(e[+-]?[\d]+)?/i ) ||
        (rb = str =~ /^([\d]+|[\d]+[\d_]*[\d]+)(.([\d]+|[\d]+[\d_]*[\d]+))?(e[+-]?([\d]+|[\d]+[\d_]*[\d]+))?/i )
      unless ok
        raise ArgumentError, "invalid value for Float(): #{obj.inspect}"
      end
      idxu = str.__indexOfByte( ?_.ord , 1 ) # arg/result is one-based
      unless idxu._equal?(0)
        if idxu._equal?(str.size ) ||
            (rc = str =~ /([\d]+_e)|(e_)/i )
          raise ArgumentError, "invalid value for Float(): #{obj.inspect}"#
        end
        str = str.dup.delete('_')
      end
      f = str.__to_f
      if f.nan?
        raise ArgumentError, "invalid value for Float(): #{obj.inspect}"
      end
      return f * sign
    end
    Maglev::Type.coerce_to(obj, Float, :to_f)
  end

  module_function :Float

  def Integer(obj)
    if (obj._isInteger)
      return obj
    end
    if obj._isString
      if obj.__indexOfByte( 0 , 1 )._not_equal?(0)
        raise ArgumentError, 'null character in arg to Integer()'
      end
      str = obj.lstrip  # remove leading white space
      str.rstrip! # remove trailing white space, in place
      if str.size._equal?(0)
        raise ArgumentError, "invalid value for Integer: (empty string)"
      end
      if str[0]._equal?( ?_ ) || str[-1]._equal?( ?_ )
        raise ArgumentError, "invalid value for Integer: #{obj.inspect}"
      end
      return str.to_inum(0, true)
    end
    if obj._equal?(nil)
      return 0
    end
    val = nil
    begin
      val = obj.to_int
    rescue Exception
    end
    if val._equal?(nil)
      begin
        val = obj.to_i
      rescue Exception
      end
    end
    unless val._isInteger
      raise TypeError, 'Coercion error: to_int or to_i did not return an Integer'
    end
    val
  end
  module_function :Integer

  def Array(obj)
    if obj._isArray
      return obj
    end
    bad = false
    begin
      res = obj.to_ary
      if res._isArray
        return res
      else
        bad = res._not_equal?(nil)
      end
    rescue
      # ignore
    end
    if bad
      raise TypeError, 'Kernel#Array, to_ary did not return an Array'
    end
    Maglev::Type.coerce_to(obj, Array, :to_a)
  end
  module_function :Array

  def String(obj)
    Maglev::Type.coerce_to(obj, String, :to_s)
  end

  module_function :String

  ##
  # MRI uses a macro named NUM2DBL which has essentially the same semantics as
  # Float(), with the difference that it raises a TypeError and not a
  # ArgumentError. It is only used in a few places (in MRI and Rubinius).
  #--
  # If we can, we should probably get rid of this.

  def FloatValue(obj)
    begin
      Float(obj)
    rescue
      raise TypeError, 'no implicit conversion to float'
    end
  end
  #  private :FloatValue   # TODO: uncomment

  def warn(warning)
    $stderr.puts warning unless $VERBOSE.nil?
    nil
  end
  module_function :warn

  def __cext_warning(warning)
    # called by  rb_warning
    warn(warning)
  end

  # from timeout.rb
  ##
  # Identical to:
  #
  #   Timeout::timeout(n, e, &block).
  #
  # Defined for backwards compatibility with earlier versions of timeout.rb, see
  # Timeout#timeout.
  def timeout(n, e = nil, &block) # :nodoc:
    Timeout::timeout(n, e, &block)
  end

  def fork(&block)
    Process.fork(&block)
  end
end
