# ---------------------------------
#   Proc 

class VariableContext
  # creating subclasses of VariableContext is not allowed
  # extending VariableContext is not allowed outside of bootstrap
  primitive_nobridge 'at', 'at:'
  primitive_nobridge 'put', 'at:put:'

  def __evVcGput(tilde, underscore)
    # for use by eval implementation only
         tilde.__storeRubyVcGlobal(0x70)
    underscore.__storeRubyVcGlobal(0x71)
    self
  end
end

class ExecBlock
  # creating subclasses of ExecBlock is not allowed
  # extending ExecBlock is not allowed outside of bootstrap
  primitive_nobridge '__num_args', 'numArgs'
  primitive_nobridge '__last_star', 'lastRubyArgIsStar'
  primitive_nobridge '__no_declared_args', 'noRubyDeclaredArgs'

  primitive_nobridge '__fetchRubyVcGlobal', '_rubyVcGlobalAt:'
  primitive_nobridge '__setRubyVcGlobal', '_rubyVcGlobalAt:put:'

  primitive_nobridge '__copy_for_proc', '_copyForProc:'
    #  one arg,  
    #    0 == for lambda, 
    #    2 == for non-lambda  proc 

  primitive_nobridge '__source_location', '_fileAndLine'

  primitive_nobridge '__set_self', 'setSelf:'
  primitive_nobridge '__get_self' , 'selfValue'

  # call, call:, call::, call::: , call&, call:&, call::&
  #  will be compiled to special bytecodes
  #  and won't use the bridge methods generated for  call*

  # bridge methods generated for call*  will be used but the actual
  #   send of call* will be a special bytecode.
  primitive          'call*' , '_rubyCall:'

    def [](*args, &block)
      self.call(*args, &block)
    end

    # optimize common variants to avoid bridge method 
    def []
      self.call
    end

    def [](a)  # also used by Cext implementation
      self.call(a)
    end

    def [](a, b)
      self.call(a, b)
    end

    def [](a, b, c)
      self.call(a, b, c)
    end

    def [](*args)  # also used by Cext implementation
      self.call(*args)
    end

    def [](&block)
      self.call(&block)
    end

    def [](a, &block)
      self.call(a, &block)
    end

    def [](a, b, &block)
      self.call(a, b, &block)
    end

    def [](a, b, c, &block)
      self.call(a, b, c, &block)
    end

    def __signal_callback
      # used by Signal#trap implemenation
      self.call
    end

    def __fficallback(*args)
      # execution of an ExecBlock by an FFI callback invokes this method
      # also used by Cext
      self.call(*args)
    end
 
    def arity
      na = self.__num_args 
      if na._equal?(0)
        if self.__no_declared_args
          na = -1  # for Proc.new { }.arity == -1
        end 
      else
        if self.__last_star
          na = -(na)  # negated (num required args + 1)
        end
      end
      na
    end

    primitive_nobridge 'inspect', '_rubyInspect' 

    def to_proc
      Proc.new(self)
    end
    def __to_proc
      # invoked from generated code when incoming block argument is
      # used other than passing as value for & suffix char of a method call.
      # The result of __to_proc is cached in an evaluation temporary generated
      # as a method temp by the AST to IR transformation.
      # Exceptions to automatic __to_proc coercion:
      #   a) any code compiled as bootstrap code. In bootstrap code __to_proc
      #      must be coded explicitly
      #   b) 'NoToProcSelectors' defined by RubyAbstractCallNode>>initialize ,
      #      defines private selectors for which receiver and/or last arg is
      #      not automatically coerced.
      Proc.new(self)
    end
    def __to_proc_arg
      self
    end
end


class Proc 
   # Proc is identically  the smalltalk class RubyProc  
    def self.new(*args, &block)
      if block._isBlock
        inst = self.allocate
        b = block.__copy_for_proc(2) # transform break bytecodes if any
        b.freeze
        inst.__initialize(&b)
        inst.initialize(*args)
        return inst
      elsif block._is_a?(Proc)
        raise ArgumentError, 'too many args , block arg is already a Proc'
      else
        raise ArgumentError, 'tried to create Proc object without a block' 
      end
      
    end

    def self.new(&block)
      # optimize common variant to avoid bridge methods
      if block._isBlock
        inst = self.allocate
        b = block.__copy_for_proc(2) # transform break bytecodes if any
        b.freeze
        inst.__initialize(&b)
        inst.initialize
        return inst
      elsif block._is_a?(Proc)
        return block
      else
        raise ArgumentError, 'tried to create Proc object without a block' 
      end
    end

    def self.new(block)
      # optimize common variant to avoid bridge methods
      if block._isBlock
        inst = self.allocate
        b = block.__copy_for_proc(2) # transform break bytecodes if any
        b.freeze
        inst.__initialize(&b)
        inst.initialize
        return inst
      else
        raise ArgumentError, 'tried to create Proc object without a block'
      end
    end

    def self.new_lambda(&block)
      if block._isBlock
        inst = self.allocate
        b = block.__copy_for_proc(0)
        b.freeze
        inst.__initialize(&b)
        return inst
      elsif block._is_a?(Proc)
        pb = block.__block
        b = pb.__copy_for_proc(0)
        b.freeze
        if b._equal?(pb)
          return block  # the argument block  is already a lambda
        else
          inst = self.allocate
          inst.__initialize(&b) 
          return inst
        end
      else
        raise ArgumentError, 'tried to create Proc object without a block'
      end
    end

    def self.new_lambda
      # no bridge methods
      raise ArgumentError, 'tried to create Proc object without a block'
    end

    def __block
      @_st_block
    end

    def __initialize(&block_arg)
      @_st_block = block_arg
    end

    # private primitives for $~ implementation only
    def __fetchRubyVcGlobal(ofs)
      @_st_block.__fetchRubyVcGlobal(ofs)
    end
    def __setRubyVcGlobal(ofs, val)
      @_st_block.__setRubyVcGlobal(ofs, val)
    end

    def __set_self(obj)
      @_st_block.__set_self(obj)
    end

    primitive_nobridge '__get_self' , 'selfValue'

    def [](*args, &block)
      @_st_block.call(*args, &block)
    end

    # optimize common variants to avoid bridge methods
    def []
      @_st_block.call
    end

    def [](a)
      @_st_block.call(a)
    end

    def [](a, b)
      @_st_block.call(a, b)
    end

    def [](a, b, c)
      @_st_block.call(a, b, c)
    end

    def [](&block)
      @_st_block.call(&block)
    end

    def [](a, &block)
      @_st_block.call(a, &block)
    end

    def [](a, b, &block)
      @_st_block.call(a, b, &block)
    end

    def [](a, b, c, &block)
      @_st_block.call(a, b, c, &block)
    end

    def __fficallback(*args)
      # execution of a Proc by an FFI callback invokes this method
      @_st_block.call(*args)
    end

    def __signal_callback
      # used by Signal#trap implemenation
      @_st_block.call
    end

    def ==(other)
      if other._is_a?(Proc)
        return @_st_block._equal?(other.__block)
      end
      false
    end

    def __arity=(v)
      @_st_arity = v
    end

    def arity
      na = @_st_arity
      if na._not_equal?(nil)
        return na
      end
      blk = @_st_block
      na = blk.__num_args 
      if na._equal?(0)
        if blk.__no_declared_args
          na = -1  # for Proc.new { }.arity == -1
        end 
      else
        if blk.__last_star
          na = -(na)  # negated (num required args + 1)
        end
      end
      na
    end

    def call(*args, &block)
      @_st_block.call(*args, &block)
    end

    # optimize common variants to avoid bridge methods
    def call
      @_st_block.call
    end
    def call(a)
      @_st_block.call(a)
    end
    def call(a, b)
      @_st_block.call(a, b)
    end
    def call(a, b, c)
      @_st_block.call(a, b, c)
    end
    def call(&block)
      @_st_block.call(&block)
    end
    def call(a, &block)
      @_st_block.call(a, &block)
    end
    def call(a, b, &block)
      @_st_block.call(a, b, &block)
    end
    def call(a, b, c, &block)
      @_st_block.call(a, b, c, &block)
    end

    #  Creating a Binding from a Proc is not yet supported.
    #  The block from which the Proc was created does not
    #  necessarily have a VariableContext so it may not even
    #  be possible to create a complete Binding.
    def binding(*args, &block)
      # send of :binding not supported 
      raise NotImplementedError , 'Proc#binding not supported'
    end
    def binding(__lex_path)
      # __lex_path arg is generated by the parser
      raise NotImplementedError , 'Proc#binding not supported'
    end

    def inspect
      "#<Proc>"
    end

    def source_location 
      # exact location not implemented yet .
      # result is an approximation, the file and line of the
      #  home method of the block.
      @_st_block.__source_location
    end

    def to_proc
      self
    end
    def __to_proc
      self
    end
    def __to_proc_arg
      self
    end

    def value(a)
      # used by smalltalk rubyEval*
      @_st_block.call(a)
    end

    def value
      # used by smalltalk _rubyLoop*
      @_st_block.call
    end

end
