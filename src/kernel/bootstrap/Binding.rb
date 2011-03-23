class Binding
  # Binding is identically the Smalltalk class  RubyBinding
 
  class_primitive_nobridge '__build_names', '_bindingInfo:'

  def initialize( binding_context , obj, blk )
    info = Binding.__build_names( binding_context )
    @_st_staticLink = info[0]   # staticLink may be nil
    @_st_names = info[1]
    @_st_selfObj = obj 
    @_st_block = blk
    @_st_forModuleEval = false
    @_st_tmpsDict = nil
    @_st_homeMeth = binding_context[-1]	# for 1.8.7
  end

  def self.__basic_new(a_self)
    bnd = self.allocate
    bnd.__init(a_self)
  end

  def __init(a_self)
    @_st_selfObj = a_self
    @_st_block = nil
    @_st_forModuleEval = false
    @_st_tmpsDict = nil
    @_st_names = nil
    @_st_staticLink = nil
    @_st_lexicalScope = nil
    self
  end

  def __set_lex_scope(a_scope)
    unless a_scope._is_a?(LexicalPath)
      raise ArgumentError, 'expected instance of Binding::LexicalPath; send of :binding or :eval not supported'
    end
    @_st_lexicalScope = a_scope
  end

  def __home_method
    @_st_homeMeth  # for 1.8.7
  end

  def block
    @_st_block
  end

  def eval(*args, &block_arg)		# added for 1.8.7
    # should always come here via bridge method
    # IR generator synthesizes missing args as needed per Kernel.eval
    nargs = args.size
    if nargs < 1
      raise ArgumentError, 'too few args, send of :eval not supported'
    end
    if nargs > 4
      raise ArgumentError, 'too many args'
    end
    # lex_path = self.__getRubyVcGlobal(0x32) # not used, use path already in self
    str = args[0]
    bnd = self
    file = args[1]
    if file._equal?(nil)
      file = args[2] # synthesized
      line = args[3] 
    else
      line = args[2]
    end
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
    @_st_staticLink # a VariableContext
  end

end
