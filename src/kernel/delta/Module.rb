class Module

  #  Class methods

  def self.constants
    puts "== WARN: STUB: MNI: Module.constants"
    []
  end

  def self.nesting
    puts "== WARN: STUB: MNI: Module.nesting"
    []
  end

  # MNI: new

  #  Instance methods

  def <(other)
    puts "== WARN: STUB: MNI: Module#<"
    false
  end

  def <=(other)
    puts "== WARN: STUB: MNI: Module#<="
    false
  end

  def >(other)
    puts "== WARN: STUB: MNI: Module#>"
    puts "== WARN: STUB: MNI: "
    false
  end

  def >=(other)
    puts "== WARN: STUB: MNI: Module#>="
    puts "== WARN: STUB: MNI: "
    false
  end

  def <=>(other)
    puts "== WARN: STUB: MNI: Module#<=>"
    puts "== WARN: STUB: MNI: "
    false
  end

  def ===(other)
    puts "== WARN: STUB: MNI: Module#==="
    puts "== WARN: STUB: MNI: "
    false
  end

  def ancestors
    puts "== WARN: STUB: MNI: Module#ancestors"
    []
  end

  def autoload(name, file_name)
    puts "== WARN: STUB: MNI: Module#autoload"
    nil
  end

  def autoload?(name)
    puts "== WARN: STUB: MNI: Module#autoload?"
    false
  end

  # TODO: Module#module_eval: move the implementation of
  # Class#module_eval from Class.rb here.
  def module_eval
    puts "== WARN: STUB: MNI: Module#module_eval"
  end

  alias class_eval module_eval

  # class_variables inherited from Behavior

  def clone
    puts "== WARN: STUB: MNI: Module#clone"
    nil
  end

  def const_missing(symbol)
    puts "== WARN: STUB: MNI: Module#const_missing"
    nil
  end

  def include?(mod)
    puts "== WARN: STUB: MNI: Module#include?"
    false
  end

  def included_modules
    puts "== WARN: STUB: MNI: Module#included_modules"
    []
  end

  def instance_method(symbol)
    puts "== WARN: STUB: MNI: Module#instance_method"
    nil
  end

  def instance_methods(inc_super=true)
    puts "== WARN: STUB: MNI: Module#instance_methods"
    []
  end

  def method_defined?(symbol)
    puts "== WARN: STUB: MNI: Module#symbol"
    false
  end

  # module_eval is above

  def private_class_method(*symbols)
    puts "== WARN: STUB: MNI: Module#private_class_method"
    nil
  end

  def private_instance_mtehods(inc_super=true)
    puts "== WARN: STUB: MNI: Module#private_instance_mtehods"
    []
  end

  def private_method_defined?(symbol)
    puts "== WARN: STUB: MNI: Module#private_method_defined?"
    false
  end

  def protected_instance_methods(inc_super=true)
    puts "== WARN: STUB: MNI: Module#protected_instance_methods"
    []
  end

  def protected_method_defined?(symbol)
    puts "== WARN: STUB: MNI: Module#protected_method_defined?"
    false
  end

  def public_class_method(*symbols)
    puts "== WARN: STUB: MNI: Module#public_class_method"
    nil
  end

  def public_instance_methods(inc_super=true)
    puts "== WARN: STUB: MNI: Module#public_instance_methods"
    []
  end

  def public_method_defined?(symbol)
    puts "== WARN: STUB: MNI: Module#public_method_defined?"
    false
  end

  # private instance methods
  # MNI alias_method
  #    needs to call Behavior>>rubyAliias:from:

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

  def private(*symbols)
    puts "== WARN: STUB: MNI: Module#private"
    # MNI
  end

  def protected(*symbols)
    puts "== WARN: STUB: MNI: Module#protected"
    # MNI
  end

  def public(*symbols)
    puts "== WARN: STUB: MNI: Module#public"
    # MNI
  end

  def remove_class_variable(symbol)
    puts "== WARN: STUB: MNI: Module#remove_class_variable"
    # MNI
  end

  def remove_method(symbol)
    puts "== WARN: STUB: MNI: Module#remove_method"
    # MNI
  end

  def undef_method(symbol)
    puts "== WARN: STUB: MNI: Module#undef_method"
    # MNI
  end

end
