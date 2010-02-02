class Binding
  # Binding is identically the Smalltalk class  RubyBinding
 
  class_primitive_nobridge '__build_names', '_bindingInfo:'

  def initialize( binding_context , obj, blk )
    info = Binding.__build_names( binding_context )
    @staticLink = info[0]   # staticLink may be nil
    @names = info[1]
    @selfObj = obj 
    @block = blk
    @forModuleEval = false
    @tmpsDict = nil
    # if this code changed see also RubyBinding>>ctx:mainSelf: 
  end

  def self.__basic_new(a_self)
    bnd = self.allocate
    bnd.__init(a_self)
  end

  def __init(a_self)
    @selfObj = a_self
    @block = nil
    @forModuleEval = false
    @tmpsDict = nil
    @names = []
    @staticLink = nil
    @lexicalScope = nil
    self
  end

  def __set_lex_scope(a_scope)
    unless a_scope._is_a?(LexicalPath)
      raise ArgumentError, 'expected instance of Binding::LexicalPath; send of :binding or :eval not supported'
    end
    @lexicalScope = a_scope
  end

  def block
    @block
  end

  # eval support methods
  def __context
    @staticLink # a VariableContext
  end

  def __get_temp( symbol )
    # only used for temps not in the starting VariableContext
    @tmpsDict[ symbol ]
  end

  def __put_temp( symbol, value )
    # returns value
    # only used for temps not in the starting VariableContext
    h = @tmpsDict
    if h._equal?(nil)
      h = IdentityHash.new
      @tmpsDict = h
    end
    unless h.has_key?( symbol )
      (nms = @names) << symbol 
      nms << nil 
    end
    h[ symbol ] = value
    value 
  end

end
