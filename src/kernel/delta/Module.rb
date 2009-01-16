class Module

  #  Class methods

  def self.constants
    _stub_warn("Module.constants")
    []
  end

  def self.nesting
    _stub_warn("Module.nesting")
    []
  end

  # MNI: new

  #  Instance methods

  def <(other)
    _stub_warn("Module#<")
    false
  end

  def <=(other)
    _stub_warn("Module#<=")
    false
  end

  def >(other)
    _stub_warn("Module#>")
    false
  end

  def >=(other)
    _stub_warn("Module#>=")
    false
  end

  def <=>(other)
    _stub_warn("Module#<=>")
    false
  end

  def ===(other)
    _stub_warn("Module#===")
    false
  end

  def ancestors
    _stub_warn("Module#ancestors")
    []
  end

  # class_variables inherited from Behavior

  def clone
    _stub_warn("Module#clone")
    nil
  end

  def include?(mod)
    _stub_warn("Module#include?")
    false
  end

  def included_modules
    _stub_warn("Module#included_modules")
    []
  end

  def instance_methods(inc_super=true)
    _stub_warn("Module#instance_methods")
    []
  end

  def private_class_method(*symbols)
    _stub_warn("Module#private_class_method")
    nil
  end

  def private_instance_methods(inc_super=true)
    _stub_warn("Module#private_instance_mtehods")
    []
  end

  def protected_instance_methods(inc_super=true)
    _stub_warn("Module#protected_instance_methods")
    []
  end

  def public_class_method(*symbols)
    _stub_warn("Module#public_class_method")
    nil
  end

  def public_instance_methods(inc_super=true)
    _stub_warn("Module#public_instance_methods")
    []
  end

  # private instance methods

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

  def remove_class_variable(symbol)
    _stub_warn("Module#remove_class_variable")
  end

  def remove_method(symbol)
    _stub_warn("Module#remove_method")
  end

  def undef_method(symbol)
    _stub_warn("Module#undef_method")
  end

end
