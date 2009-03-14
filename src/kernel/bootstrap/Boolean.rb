class Boolean

#---------------------------------
#   NilTF.gs has implementation of Boolean>>class
#     which returns either TrueClass or FalseClass

  primitive_nobridge '_isSpecial', 'isSpecial'

  primitive_nobridge '^', '_rubyXor:'

  primitive_nobridge '&', '_rubyAnd:'

  primitive_nobridge '|' , '_rubyOr:'

  primitive_nobridge 'not'

  def inspect(touchedSet=nil)
    to_s
  end

  def to_s
    self.equal?(true) ? "true" : "false"
  end
end
