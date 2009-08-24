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
  primitive_nobridge '_numArgs', 'numArgs'
  primitive_nobridge '_lastStar', 'lastRubyArgIsStar'
  primitive_nobridge '_noDeclaredArgs', 'noRubyDeclaredArgs'

  primitive_nobridge '_fetchRubyVcGlobal', '_rubyVcGlobalAt:'
  primitive_nobridge '_setRubyVcGlobal', '_rubyVcGlobalAt:put:'

  primitive_nobridge '_copyForRuby', '_copyForRuby:'
    #  one Fixnum arg,  0 == for lambda, 1 == for define_method

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
      na = self._numArgs 
      if na.equal?(0)
        if (self._noDeclaredArgs)
          na = -1  # for Proc.new { }.arity == -1
        end 
      else
        if (self._lastStar) 
          na = -(na)  # negated (num required args + 1)
        end
      end
      na
    end

    primitive_nobridge 'inspect', '_rubyInspect' 

    def to_proc
      Proc.new(self)
    end
    def _to_proc
      Proc.new(self)
    end
end


class Proc 
   # Proc is identically  the smalltalk class RubyProc  
    def self.new(&blk)
      if blk._isBlock
        inst = self.allocate
        inst._initialize(&blk)
        inst.initialize
        return inst
      elsif blk.is_a?(Proc)
        return blk
      else
        raise ArgumentError, 'tried to create Proc object without a block' 
      end
    end

    def self.new(blk)
      if blk._isBlock
        inst = self.allocate
        inst._init_from_block(blk)
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
        b = blk._copyForRuby(0)
        inst._initialize(&b)
        return inst
      elsif blk.is_a?(Proc)
        pb = blk._block
        b = pb._copyForRuby(0)
        if (b.equal?(pb)) 
          return blk  # the argument blk  is already a lambda
        else
          inst = self.allocate
          inst._initialize(&b) 
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

    def _block
      @block
    end

    def _initialize(&blk)
      @block = blk
    end

    def _init_from_block(blk)
      @block = blk
    end

    # private primitives for $~ implementation only
    def _fetchRubyVcGlobal(ofs)
      @block._fetchRubyVcGlobal(ofs)
    end
    def _setRubyVcGlobal(ofs, val)
      @block._setRubyVcGlobal(ofs, val)
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
      if other.is_a?(Proc)
        return @block.equal?(other._block)
      end
      false
    end

    def _arity=(v)
      @arity = v
    end

    def arity
      na = @arity
      if na._not_equal?(nil)
        return na
      end
      blk = @block
      na = blk._numArgs 
      if na.equal?(0)
        if (blk._noDeclaredArgs)
          na = -1  # for Proc.new { }.arity == -1
        end 
      else
        if (blk._lastStar) 
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

    # TODO: binding
    #  creating a Binding from a Proc is not yet supported.
    #  The block from which the Proc was created does not
    #  necessarily have a VariableContext so it may not even
    #  be possible to create a complete Binding.

    def inspect
      "#<Proc>"
    end

    def to_proc
      self
    end
    def _to_proc
      self
    end

    def value
      # used by smalltalk rubyEval:
      @block.call
    end
end
