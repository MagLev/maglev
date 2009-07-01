class UnboundMethod
  # UnboundMethod is identically Smalltalk RubyUnboundMeth

  # Maglev implementation differs from MRI in that Method is a
  #  subclass of UnboundMethod, not of Object,

  primitive_nobridge 'arity', 'arity'
  primitive_nobridge '_bind', 'bind:'
  primitive_nobridge '_home_class', 'homeClass'

  def bind(obj)
    hm_cls = self._home_class 
    if (obj.kind_of?( hm_cls))
      return _bind(obj)   # returns a Method
    else
      raise TypeError , ('obj must be kind_of ' << (hm_class.name ))
    end
  end

end
