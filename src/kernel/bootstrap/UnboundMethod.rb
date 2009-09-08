class UnboundMethod
  # UnboundMethod is identically Smalltalk RubyUnboundMeth

  # Maglev implementation differs from MRI in that Method is a
  #  subclass of UnboundMethod, not of Object,

  primitive_nobridge 'arity', 'arity'
  primitive_nobridge '_bind', 'bind:'
  primitive_nobridge '_home_class', 'homeClass'

  primitive_nobridge '_selector_prefix', '_selectorPrefix'

  def _gsmeth
    @gsmeth 
  end
  def _nonbridge_meth
    @nonBridgeMeth
  end

  def ==(other)
    other.class.equal?(self.class) &&
    other.arity == @arity && 
    (other._gsmeth.equal?(@gsmeth) || other._nonbridge_meth.equal?(@nonBridgeMeth))
  end

  def to_s
    str = '#<'
    str << self.class.name
    str << ': '
    str << self._home_class.name
    str << ?#
    str << self._selector_prefix
    str << ?>
    str
  end

  def inspect
    self.to_s
  end

  def bind(obj)
    hm_cls = self._home_class 
    if (obj.kind_of?( hm_cls))
      return _bind(obj)   # returns a Method
    else
      raise TypeError , ('obj must be kind_of ' << (hm_cls.name ))
    end
  end

end
