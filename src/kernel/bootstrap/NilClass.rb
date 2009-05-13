class NilClass
  primitive_nobridge '_isSpecial', 'isSpecial'

  primitive_nobridge '&', '_rubyAnd:'
  #  For receiver nil,  Or and Xor are the same
  primitive_nobridge '^', '_rubyOr:'
  primitive_nobridge '|', '_rubyOr:'

  primitive 'nil?' , '_rubyNilQ'
  primitive 'to_a' , '_ruby_to_a'

  primitive 'to_f' , '_ruby_to_f'
  primitive 'to_i' , '_ruby_to_i'
  primitive 'to_s' , '_ruby_to_s'

  def frozen?
    false
  end

  def tainted?
    false
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

  def _plus_match
    nil
  end

  def _to_proc
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

