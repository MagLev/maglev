class Method
    # Method is identically Smalltalk RubyMethod  .
    #   RubyMethod is defined in the .mcz 

    def self.name
      'Method'
    end

    primitive_nobridge 'call', 'call'
    primitive_nobridge 'call:', 'call:'
    primitive_nobridge 'call::', 'call:with:'
    primitive_nobridge 'call:::', 'call:with:with:'
    primitive_nobridge  'call*' , 'callStar:' 

    primitive_nobridge '[]', 'call'
    primitive_nobridge '[]:', 'call:'
    primitive_nobridge '[]::', 'call:with:'
    primitive_nobridge '[]:::', 'call:with:with:'
    primitive_nobridge  '[]*' , 'callStar:' 

    primitive_nobridge '_receiver', 'receiver'

    primitive_nobridge '_selector', 'selector'

    def ==(other)
      # Returns true if other is the same method as self
      if (other.kind_of?(Method))
        return @receiver.equal?(other._receiver) && 
		@baseSelector.equal?(other._selector)
      else
        return false
      end
    end

    # arity inherited from UnboundMethod

    def eql?(other)
      self.==(other)
    end
   
    primitive 'unbind'

    # TODO to_proc
    
end
