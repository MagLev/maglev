# ---------------------------------
#   Proc 

class VariableContext
  # creating subclasses of VariableContext is not allowed
  # extending VariableContext is not allowed outside of bootstrap
  primitive_nobridge 'at', 'at:'
  primitive_nobridge 'put', 'at:put:'
end

class ExecBlock
  # creating subclasses of ExecBlock is not allowed
  # extending ExecBlock is not allowed outside of bootstrap
  primitive_nobridge '__num_args', 'numArgs'
  primitive_nobridge '__last_star', 'lastRubyArgIsStar'
  primitive_nobridge '__no_declared_args', 'noRubyDeclaredArgs'

  primitive_nobridge '__fetchRubyVcGlobal', '_rubyVcGlobalAt:'
  primitive_nobridge '__setRubyVcGlobal', '_rubyVcGlobalAt:put:'

  primitive_nobridge '__copy_for_ruby', '_copyForRuby:'
    #  one Fixnum arg,  0 == for lambda, 1 == for define_method , 
    #     2 == for non-lambda  proc 

  # call, call:, call::, call::: will be compiled to special bytecodes
  #  and won't use the bridge methods generated for  call*

  # bridge methods generated for call*  will be used but the actual
  #   send of call* will be a special bytecode.
  primitive          'call*' , '_rubyCall:'

    def []
      self.call
    end

    def [](a)
      self.call(a)
    end

    def [](a, b)
      self.call(a, b)
    end

    def [](a, b, c)
      self.call(a, b, c)
    end

    def [](*args)
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
      # invoked from generated code
      Proc.new(self)
    end
end


class Proc 
   # Proc is identically  the smalltalk class RubyProc  
    def self.new(&blk)
      if blk._isBlock
        inst = self.allocate
        b = blk.__copy_for_ruby(2) # transform break bytecodes if any
        b.freeze
        inst.__initialize(&b)
        inst.initialize
        return inst
      elsif blk._is_a?(Proc)
        return blk
      else
        raise ArgumentError, 'tried to create Proc object without a block' 
      end
    end

    def self.new(blk)
      if blk._isBlock
        inst = self.allocate
        b = blk.__copy_for_ruby(2) # transform break bytecodes if any
        b.freeze
        inst.__initialize(&b)
        inst.initialize
        return inst
      else
        raise ArgumentError, 'tried to create Proc object without a block'
      end
    end


    def self.new
      # no bridge methods
      raise ArgumentError, 'tried to create Proc object without a block'
    end

    def self.new_lambda(&blk)
      if blk._isBlock
        inst = self.allocate
        b = blk.__copy_for_ruby(0)
        b.freeze
        inst.__initialize(&b)
        return inst
      elsif blk._is_a?(Proc)
        pb = blk.__block
        b = pb.__copy_for_ruby(0)
        b.freeze
        if b._equal?(pb)
          return blk  # the argument blk  is already a lambda
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
      @block
    end

    def __initialize(&blk)
      @block = blk
    end

    # private primitives for $~ implementation only
    def __fetchRubyVcGlobal(ofs)
      @block.__fetchRubyVcGlobal(ofs)
    end
    def __setRubyVcGlobal(ofs, val)
      @block.__setRubyVcGlobal(ofs, val)
    end

    def []
      @block.call
    end

    def [](a)
      @block.call(a)
    end

    def [](a, b)
      @block.call(a, b)
    end

    def [](a, b, c)
      @block.call(a, b, c)
    end

    def [](*args)
      @block.call(*args)
    end

    def ==(other)
      if other._is_a?(Proc)
        return @block._equal?(other.__block)
      end
      false
    end

    def __arity=(v)
      @arity = v
    end

    def arity
      na = @arity
      if na._not_equal?(nil)
        return na
      end
      blk = @block
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

    def call(*args)
      @block.call(*args)
    end
    def call
      # this and following variants get no bridge methods
      @block.call
    end
    def call(a)
      @block.call(a)
    end
    def call(a, b)
      @block.call(a, b)
    end
    def call(a, b, c)
      @block.call(a, b, c)
    end

    #  Creating a Binding from a Proc is not yet supported.
    #  The block from which the Proc was created does not
    #  necessarily have a VariableContext so it may not even
    #  be possible to create a complete Binding.
    def binding
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

    def to_proc
      self
    end
    def __to_proc
      self
    end

    def value
      # used by smalltalk rubyEval:
      @block.call
    end

    def value(a)
      # used by smalltalk  Integer>>_rubyTimes...
      @block.call(a)
    end
end
