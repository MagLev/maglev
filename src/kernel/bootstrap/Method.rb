class GsNMethod
   # Gemstone compiled methods  are instances of GsNMethod
   #  subclasses of GsNMethod is not allowed
   #  extending GsNMethod is not allowed outside of bootstrap

   primitive_nobridge '_call_star*&' , '_executeInContext:star:block:'

   primitive_nobridge 'inspect', '_rubyInspect' 
end

class Method
    # Method is identically Smalltalk RubyMeth
    #   RubyMethod is defined in the .mcz

    def _obj
      @obj
    end

    def ==(other)
      # Returns true if other is the same method as self
      if (other.kind_of?(Method))
        return @obj.equal?(other._obj) &&
         @gsmeth.equal?(other._gsmeth)
      else
        return false
      end
    end

    # arity inherited from UnboundMethod

    def call(*args, &blk)
      @gsmeth._call_star(@obj, *args, &blk)
    end

    def [](*args, &blk)
      @gsmeth._call_star(@obj, *args, &blk)
    end

    alias_method :eql? , :==

    def to_proc
      p = Proc.new { |*args| self.call(*args) }
      p._arity=( self.arity )
      p
    end

    primitive_nobridge 'unbind', 'unbind'
end
