class Module

  #  Class methods

  def self.constants
    []
  end

  def self.nesting
    []
  end

  # MNI: new

  #  Instance methods

  def <(other)
    false
  end

  def <=(other)
    false
  end

  def >(other)
    false
  end

  def >=(other)
    false
  end

  def <=>(other)
    false
  end

  def ===(other)
    false
  end

  def ancestors
    []
  end

  def autoload(name, file_name)
    nil
  end

  def autoload(name)
    false
  end

  # TODO: Module#module_eval: move the implementation of
  # Class#module_eval from Class.rb here.
  def module_eval
  end

  alias class_eval module_eval

  # class_variables inherited from Behavior

  def clone
    nil
  end

  def const_defined?(symbol)
    false
  end

  def const_get(symbol)
    nil
  end

  def const_missing(symbol)
    nil
  end

  def const_set(symbol, obj)
    nil
  end

  def constants
    []
  end

  def include?(mod)
    false
  end

  def included_modules
    []
  end

  def instance_method(symbol)
    nil
  end

  def instance_methods(inc_super=true)
    []
  end

  def method_defined?(symbol)
    false
  end

  # module_eval is above

  def name
    ""
  end

  def private_class_method(*symbols)
    nil
  end

  def private_instance_mtehods(inc_super=true)
    []
  end

  def private_method_defined?(symbol)
    false
  end

  def protected_instance_methods(inc_super=true)
    []
  end

  def protected_method_defined?(symbol)
    false
  end

  def public_class_method(*symbols)
    nil
  end

  def public_instance_methods(inc_super=true)
    []
  end

  def public_method_defined?(symbol)
    false
  end

  # private instance methods
  # TODO alias_method: is currently in Class.rb: move here

  # MNI: append_features
  # MNI: attr
  # MNI: attr_accessor
  # MNI: attr_reader
  # MNI: attr_writer
  # MNI: define_method
  # MNI: extend_object
  # MNI: extended
  # MNI: include
  # MNI: included

  # Invoked as a callback when a method is added to the reciever
  def method_added(symbol)
  end

  # Invoked as a callback when a method is removed from the reciever
  def method_removed(symbol)
  end

  # Invoked as a callback when a method is undefined in the reciever
  def method_undefined(symbol)
  end

  # MNI: module_function,  see old impl in Object.rb 

  # MNI: private
  # MNI: protected
  # MNI: public
  # MNI: remove_class_variable
  # MNI: remove_const
  # MNI: remove_method
  # MNI: undef_method
end
