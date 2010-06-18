class Module

  def clone
    raise NotImplementedError , "Module#clone"
  end

  # Invoked as a callback when a reference to an undefined symbol is made.
  def const_missing(symbol)
    raise NameError, "uninitialized constant #{symbol}"
  end

  def include?(other)
    unless other._is_a?(Module) && ! other._is_a?(Class)
      raise TypeError, 'Module#include? , argument not a Module'
    end
    imods = self.included_modules
    k = 0
    klim = imods.size
    while k < klim
      return true if imods[k]._equal?(other)
      k += 1
    end
    false
  end

  primitive_nobridge '__ruby_methods', 'rubyMethods:protection:'

  primitive '__ruby_instance_methods', 'rubyInstanceMethods:protection:'

  def methods(regular = true)
    set = self.__ruby_singleton_methods(false, -1)  # include protected meths
    if regular
      set =  set + self.__ruby_methods(true, -1) # include protected meths
    end
    Module.__filter_method_names(set)
  end

  def instance_methods(inc_super=true)
    # include both protected and public methods
    Module.__filter_method_names(__ruby_instance_methods(inc_super, -1))
  end

  def public_instance_methods(inc_super=true)
    Module.__filter_method_names(__ruby_instance_methods(inc_super, 0))
  end

  def private_instance_methods(inc_super=true)
    Module.__filter_method_names(__ruby_instance_methods(inc_super, 2))
  end

  def protected_instance_methods(inc_super=true)
    Module.__filter_method_names(__ruby_instance_methods(inc_super, 1))
  end



  def extend_object(object)
    raise NotImplementedError, 'Module#extend_object'
  end

end
