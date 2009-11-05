class Module

  #  Class methods

  # MNI: new

  #  Instance methods

  # class_variables inherited from Behavior

  def clone
    raise NotImplementedError , "Module#clone"
  end

  def include?(other)
    unless other.is_a?(Module) && ! other.is_a?(Class)
      raise TypeError, 'Module#include? , argument not a Module'
    end
    imods = self.included_modules
    k = 0
    klim = imods.size
    while k < klim
      return true if imods[k].equal?(other)
      k += 1
    end
    false
  end

  primitive '__ruby_methods_protection', 'rubyMethods:protection:'

  def instance_methods(inc_super=true)
    __ruby_methods_protection(inc_super, 0);
  end

  def public_instance_methods(inc_super=true)
    __ruby_methods_protection(inc_super, 0);
  end

  def private_instance_methods(inc_super=true)
    __ruby_methods_protection(inc_super, 2);
  end

  def protected_instance_methods(inc_super=true)
    __ruby_methods_protection(inc_super, 1);
  end

  # MNI: attr
  def extend_object(object)
    raise NotImplementedError, 'Module#extend_object'
  end

  # remove_method  implemented in Behavior
  # undef_method implemented in Behavior

end
