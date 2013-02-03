  # file Kernel2.rb  , parts of kernel that must be deferred to later
  #  in the bootstrap

module Kernel

  def binding(lex_path, &block)
    # lex_path arg is synthesized by the parser.
    # usually the block argument is synthesized by the parser.
    bnd = Binding.new( self.__binding_ctx(0), self , block)
    bnd.__set_lex_scope(lex_path)
    bnd 
  end

  # call-seq:
  #   binding => a_binding
  #
  # Returns a +Binding+ object, describing the variable and method bindings
  # at the point of call.
  def binding
    # could be sent via  __send__ or send, but not supported yet
    # You must code binding calls explicitly.
    raise ArgumentError, 'too few args, send of :binding not supported'
    # before fix of Trac660 this path used to create the top-level binding.
    #Binding.new( self.__binding_ctx(0), self, nil )
  end
 
  def binding(&block)
    # could be sent via  __send__ or send, but not supported yet
    # You must code binding calls explicitly.
    raise ArgumentError, 'too few args, send of :binding not supported'
  end

  def binding(lex_path)
    # lex_path arg is synthesized by the parser.
    bnd = Binding.new( self.__binding_ctx(0), self , nil)
    bnd.__set_lex_scope(lex_path)
    bnd 
  end

  module_function :binding

  # Returns +true+ if yield would execute a block in the current context.
  def block_given?(&block)
    # this implementation present so   send   will work.
    block_given?   # implemented by parser, not a recursive send
  end
  module_function :'block_given?'

  def lambda(&block)
    Proc.new_lambda(&block)
  end
  module_function :lambda

  def proc(&block)
    Proc.new_lambda(&block)  # use new_lambda here for 1.8.6 compatibility
  end
  module_function :proc

  def rand(n=0)
    limit = n.to_i.abs
    inst = RandomInstance
    if limit._equal?(0)
      inst.__float
    else
      inst.__next_int(limit) - 1
    end
  end
  module_function :rand

  def srand(a_seed=MaglevUndefined)
    inst = RandomInstance
    old_seed = inst.seed
    if old_seed._equal?(nil)
       old_seed = inst.__integer
    end
    if a_seed._equal?(MaglevUndefined)
      inst.seed(nil)
    else
      a_seed = Maglev::Type.coerce_to(a_seed, Fixnum, :to_int)
      inst.seed(a_seed)
    end
    old_seed
  end
  module_function :srand


  #  fixup module functions defined in Kernel.rb
  module_function(
    :load ,
    :abort ,
    :at_exit ,
#   :autoload ,  # Kernel.autoload is used in core/kernel/autoload_spec.rb
#   :'autoload?' ,
    :caller ,
    :catch ,
    :debugger ,
    :eval ,
    :exit ,
    :fail ,
    :format ,
    :gets ,
    :global_variables ,
    :gsub ,
    :'gsub!' ,
    :loop ,
    :method_missing ,
    :open ,
    :p ,
    :print ,
    :printf ,
    :putc ,
    :puts ,
    :raise ,
    :rand ,
    :readline ,
    :readlines ,
    :require ,
    :scan ,
    :select ,
    :sleep ,
    :split ,
    :sprintf ,
    :srand ,
    :sleep_ms ,
    :'`' ,
    :sub ,
    :'sub!' ,
    :system ,
    :test ,
    :throw ,
    :trap 
  )

  private :autoload, :autoload?
end
