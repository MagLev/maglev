class GsNMethod
  # Gemstone compiled methods  are instances of GsNMethod
  #  subclasses of GsNMethod are not allowed
  #  extending GsNMethod is not allowed outside of bootstrap

  primitive_nobridge '__call_star*&' , '_executeInContext:star:block:'
  primitive_nobridge '__call_star*&' , '_executeInContext:nonBridgeMeth:star:block:'
  # primitive_nobridge '__call_star*' , '_executeInContext:args:' # invoked from generated bridge only
  primitive_nobridge 'inspect', '_rubyInspect'
  primitive_nobridge '__name', '_rubyName'
  primitive_nobridge '__source_location', '_fileAndLine'
  primitive_nobridge '__source_string', 'sourceString'
  primitive_nobridge '__env_id', 'environmentId'
  primitive_nobridge '__home_method', 'homeMethod'
  primitive_nobridge '__in_class', 'inClass'

  primitive_nobridge 'ffi_library', '_libraryForCallout'
end

class Method
  # Method is identically Smalltalk RubyMeth
  #   RubyMethod is defined in the .mcz

  def __obj
    @_st_obj
  end

  def __to_proc
    self
  end

  def ==(other)
    # Returns true if other is the same method as self
    if (other._kind_of?(Method))
      return @_st_obj._equal?(other.__obj) &&
        @_st_gsmeth._equal?(other.__gsmeth)
    else
      return false
    end
  end

  # arity inherited from UnboundMethod

  def [](*args, &block)
    br = @_st_execBridge
    if br._not_equal?(nil)
      br.__call_star(@_st_obj, @_st_gsmeth, *args, &block)
    else
      @_st_gsmeth.__call_star(@_st_obj, *args, &block)
    end
  end

  def call(*args, &block)
    br = @_st_execBridge
    if br._not_equal?(nil)
      br.__call_star(@_st_obj, @_st_gsmeth, *args, &block)
    else
      @_st_gsmeth.__call_star(@_st_obj, *args, &block)
    end
  end

  alias_method :eql? , :==

  # name   is inherited  # for 1.8.7
  # owner  is inherited  # for 1.8.7

  def receiver  # for 1.8.7
    @_st_obj
  end

  def source_location  
    @_st_gsmeth.__source_location
  end

  def __to_proc_arg
    self
  end

  def to_proc
    n = self.arity
    if n <= 1 
      p = Proc.new { |*args| self.call(*args) }
    else
      p = Proc.new { |args| 
        if args._isArray 
          a = args[0]
          if a._isArray
            self.call(*a) 
          else
            self.call(*args)
          end
        else
          self.call(*args)
        end 
      }
    end 
    p.__arity=( n )
    p
  end

  primitive_nobridge 'unbind', 'unbind'
end
