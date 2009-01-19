class UnboundMethod
  # UnboundMethod is identically Smalltalk RubyUnboundMeth

  # Maglev implementation differs from MRI in that Method is a
  #  subclass of UnboundMethod, not of Object,

  def self.name
    'UnboundMethod'
  end

  primitive_nobridge 'arity', 'arity'
  primitive_nobridge 'bind', 'bind:'
  primitive_nobridge '_home_class', 'homeClass'

  def bind(obj)
    if (obj.kind_of?(@gsmeth._home_class))
      return _bind(obj)   # returns a Method
    else
      raise TypeError , ('obj must be kind_of ' << @rcvrClass.name )
    end
  end

end
