class UnboundMethod
  # UnboundMethod is identically Smalltalk RubyUnboundMeth

  # Maglev implementation differs from MRI in that Method is a
  #  subclass of UnboundMethod, not of Object,

  primitive_nobridge 'arity', 'arity'
  primitive_nobridge '__bind', 'bind:'

  primitive_nobridge '__home_class', 'homeClass'
  primitive_nobridge 'owner', 'homeClass'  # for 1.8.7

  def name
    @_st_selPrefix.dup
  end

  def __gsmeth
    @_st_gsmeth 
  end

  primitive '__nonbridge_meth', '_nonBridgeMeth'

  def ==(other)
    other.class._equal?(self.class) &&
    other.arity == self.arity && 
    (other.__gsmeth._equal?(@_st_gsmeth) || 
       other.__nonbridge_meth._equal?(self.__nonbridge_meth ))
  end

  def bind(obj)
    hm_cls = self.__home_class 
    if (obj._kind_of?( hm_cls))
      return __bind(obj)   # returns a Method
    else
      raise TypeError , ('obj must be kind_of ' << (hm_cls.name ))
    end
  end

  def inspect
    self.to_s
  end

  def source_location
    @_st_gsmeth.__source_location
  end

  def to_s
    str = '#<'
    str << self.class.name
    str << ': '
    str << self.__home_class.name
    str << ?#
    str << @_st_selPrefix
    str << ?>
    str
  end

end
