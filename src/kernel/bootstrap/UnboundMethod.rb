class UnboundMethod
  # UnboundMethod is identically Smalltalk RubyUnboundMethod

  # Gemstone implementation differs in that Method is a
  #  subclass of UnboundMethod, not of Object,
  #  and that binding of selector to an actual method is done
  #  by the  call method (which does a method lookup using perform)
  #  instead of by the instance creation of Method .

  def self.name
    'UnboundMethod'
  end

  primitive 'arity'

  primitive_nobridge '_bind', '_bind:'

  def bind(obj)
    if (obj.kind_of?(@rcvrClass))
      return _bind(obj)   # returns a Method
    else
      raise TypeError , ('obj must be kind_of ' << @rcvrClass.name )
    end
  end

end
