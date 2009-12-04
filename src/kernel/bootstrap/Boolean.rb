class Boolean
  # common superclass of TrueClass and FalseClass

#---------------------------------

  primitive_nobridge '__isSpecial', 'isSpecial'

  primitive_nobridge '^', '__rubyXor:'

  primitive_nobridge '&', '__rubyAnd:'

  primitive_nobridge '|' , '__rubyOr:'

  primitive_nobridge 'not'

  def inspect
    self.to_s
  end

  def to_s
    self._equal?(true) ? "true" : "false"
  end

  def frozen?
    false
  end

  def freeze
    # no-op
    self
  end

  def tainted?
    false
  end

  def taint
    # no-op
    self
  end

  def untaint
    # no-op
    self
  end

end

class TrueClass < Boolean
end
class FalseClass < Boolean
end
