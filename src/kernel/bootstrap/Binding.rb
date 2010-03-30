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
    @homeMeth = binding_context[-1]	# for 1.8.7
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

  def __home_method
    @homeMeth  # for 1.8.7
  end

  def block
    @block
  end

  def eval(*args, &block_arg)		# added for 1.8.7
    # should always come here via bridge method
    nargs = args.size
    if nargs < 1
      raise ArgumentError, 'too few args, send of :eval not supported'
    end
    if nargs > 3
      raise ArgumentError, 'too many args'
    end
    # lex_path = self.__getRubyVcGlobal(0x32) # not used, use path already in self
    str = args[0]
    bnd = self
    file = args[1]
    line = args[2]
    if line._equal?(nil)
      line = 0
    end
    vcgl = [ self.__getRubyVcGlobal(0x30) ,
             self.__getRubyVcGlobal(0x31) , nil ]
    bblk = bnd.block
    unless bblk._equal?(nil)
      vcgl << bblk
    end
    res = __eval_with_position(str, bnd, vcgl, file, line )
    vcgl[0].__storeRubyVcGlobal(0x30)
    vcgl[1].__storeRubyVcGlobal(0x31)
    res
  end

  
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
