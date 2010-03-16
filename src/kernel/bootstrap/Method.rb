class GsNMethod
   # Gemstone compiled methods  are instances of GsNMethod
   #  subclasses of GsNMethod is not allowed
   #  extending GsNMethod is not allowed outside of bootstrap

   primitive_nobridge '__call_star*&' , '_executeInContext:star:block:'

   primitive_nobridge 'inspect', '_rubyInspect'

   primitive_nobridge '__name', '_rubyName'
end

class Method
    # Method is identically Smalltalk RubyMeth
    #   RubyMethod is defined in the .mcz

    def __obj
      @obj
    end
    def receiver  # for 1.8.7
      @obj
    end

    def __to_proc
      self
    end

    def ==(other)
      # Returns true if other is the same method as self
      if (other._kind_of?(Method))
        return @obj._equal?(other.__obj) &&
         @gsmeth._equal?(other.__gsmeth)
      else
        return false
      end
    end

    # arity inherited from UnboundMethod

    def call(*args, &block)
      @gsmeth.__call_star(@obj, *args, &block)
    end

    def [](*args, &block)
      @gsmeth.__call_star(@obj, *args, &block)
    end

    alias_method :eql? , :==

    # name   is inherited  # for 1.8.7
    # owner  is inherited  # for 1.8.7

    def to_proc
      p = Proc.new { |*args| self.call(*args) }
      p.__arity=( self.arity )
      p
    end

    primitive_nobridge 'unbind', 'unbind'
end
