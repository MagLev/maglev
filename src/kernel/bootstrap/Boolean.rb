class Boolean
  # Boolean is the common superclass of TrueClass and FalseClass

#---------------------------------

  primitive_nobridge '__isSpecial', 'isSpecial'

  def clone  
    raise TypeError , 'cannot clone true or false'
  end

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

  def &(an_object)
    if an_object
      return true
    end
    false
  end

  def |(an_object)
    true
  end

  def ^(an_object)
    if an_object
      return false
    end
    true
  end

  def not
    false
  end
end

class FalseClass < Boolean

  def &(an_object)
    false
  end

  def |(an_object)
    if an_object
      return true
    end
    false
  end

  def ^(an_object)
    if an_object
      return true
    end
    false
  end

  def not
    true
  end
end
