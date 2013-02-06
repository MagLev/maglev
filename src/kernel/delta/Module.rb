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

  def self.__rb_path2class(path)
    if path[0] == "#"
      raise ArgumentError, "can't retrieve anonymous class #{path}"
    end

    ns = Object
    path.to_s.split('::').each do |n|
      next if n.empty?
      if ns.const_defined?(n)
	ns = ns.const_get(n)
      else
	raise ArgumentError, "undefined class/module #{n}"
      end
      unless ns._kind_of?(Module)
	raise TypeError, "#{ns} in #{path} does not refer to class/module"
      end
    end
    ns
  end

  primitive_nobridge '__transient_const_set', 'rubyTransientConst:put:block:'

  # Define a transient constant for which block will be evaluated
  # the first time the constant is referenced in the life of a VM .
  # The value of the constant will be the value returned by the block.
  # Do not use Maglev.persistent  within the block.
  def transient_const_set(name, &block)
    if name._isSymbol
      sym = name
      str = name.to_s
    else
      str = Maglev::Type.coerce_to(name, String, :to_str)
      sym = nil
    end
    unless str =~ /^[A-Z](\w)*\z/
      raise NameError, 'arg to transient_const_set? is not a valid name for a constant'
    end
    if sym._equal?(nil)
      sym = str.to_sym
    end
    val = Maglev.transient(&block)
    self.__transient_const_set(sym, val, block)
    val
  end

  def __transient_const_evaluate(&block)
    # called from Smalltalk only
    Maglev.transient(&block)
  end

end
