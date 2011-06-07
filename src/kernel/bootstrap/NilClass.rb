class NilClass
  primitive_nobridge '__isSpecial', 'isSpecial'

  def &(an_object)
    false
  end
  #  For receiver nil,  Or and Xor are the same
  def ^(an_object)
    if an_object
      return true
    end
    false
  end

  def |(an_object)
    if an_object
      return true
    end
    false
  end

  def nil?
    true
  end

  def to_a
    []
  end

  def to_f
    0.0
  end
  def to_i
    0
  end
  def to_s
    ''
  end

  def clone
    raise TypeError , 'cannot clone nil'
  end

  def frozen?
    false
  end

  def freeze
    # no-op
    nil
  end

  def gets
    # workaround for ARGF.gets unimplemented and hanging
    raise 'nil.gets not supported'
  end

  def tainted?
    false
  end

  def taint
    # no-op
    nil
  end

  def untaint
    # no-op
    nil
  end

  def inspect
    "nil"
  end

  def backtrace
    nil
  end

  # support for RubyBackRefNode productions when $~ is nil
  def pre_match
    nil
  end

  def post_match
    nil
  end

  def __plus_match
    nil
  end

  def __to_proc
    # invoked from generated code
    self
  end
  def __to_proc_arg
    self
  end

  def call(*args)
    # invoked when yield used with no block argument
    raise LocalJumpError , 'no block was passed'
  end

  private

  # This prevents infinite recursion if $_ is nil and you call
  # split(...).  Now it will raise an exception
  def split(pat, limit)
  end
end

