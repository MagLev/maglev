module Kernel
  # file Kernel2.rb  , parts of kernel that must be deferred to later
  #  in the bootstrap

  def binding
    # could be sent via  __send__ or send, but not supported yet
    # You must code binding calls explicitly.
    raise ArgumentError, 'too few args, send of :binding not supported'
    # before fix of Trac660 this path used to create the top-level binding.
    #Binding.new( self.__binding_ctx(0), self, nil )
  end
 
  def binding(&blk)
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

  def binding(lex_path, &blk)
    # lex_path arg is synthesized by the parser.
    # usually the block argument is synthesized by the parser.
    bnd = Binding.new( self.__binding_ctx(0), self , blk)
    bnd.__set_lex_scope(lex_path)
    bnd 
  end

  module_function :binding

  def block_given?(&blk)
    # this implementation present so   send   will work.
    block_given?   # implemented by parser, not a recursive send
  end
  module_function :'block_given?'

  def lambda(&blk)
    Proc.new_lambda(&blk)
  end
  module_function :lambda

  def proc(&blk)
    Proc.new_lambda(&blk)  # use new_lambda here for 1.8.6 compatibility
  end
  module_function :proc

  def rand(n=0)
    limit = n.to_i.abs
    if limit._equal?(0)
      RandomInstance.next
    else
      RandomInstance.next(limit) - 1
    end
  end
  module_function :rand

  def srand(number=nil)
    if number._equal?(nil)
      number = Time.now.to_i
    else
      number = number.to_i
    end
    old_seed = RandomInstance.seed
    RandomInstance.seed(number)
    old_seed
  end
  module_function :srand


  #  fixup module functions defined in Kernel.rb
  module_function(
    :load ,
    :abort ,
    :at_exit ,
#   :autoload ,	 # Kernel.autoload is used in core/kernel/autoload_spec.rb
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

end
