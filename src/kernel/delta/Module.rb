class Module

  #  Class methods

  GS_NOWARN = true

  def self.constants
    puts "== WARN: STUB: MNI: Module.constants" unless GS_NOWARN
    []
  end

  def self.nesting
    puts "== WARN: STUB: MNI: Module.nesting"  unless GS_NOWARN
    []
  end

  # MNI: new

  #  Instance methods

  def <(other)
    puts "== WARN: STUB: MNI: Module#<"  unless GS_NOWARN
    false
  end

  def <=(other)
    puts "== WARN: STUB: MNI: Module#<="  unless GS_NOWARN
    false
  end

  def >(other)
    puts "== WARN: STUB: MNI: Module#>"  unless GS_NOWARN
    puts "== WARN: STUB: MNI: "  unless GS_NOWARN
    false
  end

  def >=(other)
    puts "== WARN: STUB: MNI: Module#>="  unless GS_NOWARN
    puts "== WARN: STUB: MNI: "  unless GS_NOWARN
    false
  end

  def <=>(other)
    puts "== WARN: STUB: MNI: Module#<=>"  unless GS_NOWARN
    puts "== WARN: STUB: MNI: "  unless GS_NOWARN
    false
  end

  def ===(other)
    puts "== WARN: STUB: MNI: Module#==="  unless GS_NOWARN
    puts "== WARN: STUB: MNI: "  unless GS_NOWARN
    false
  end

  def ancestors
    puts "== WARN: STUB: MNI: Module#ancestors"  unless GS_NOWARN
    []
  end

  def autoload(name, file_name)
    puts "== WARN: STUB: MNI: Module#autoload"  unless GS_NOWARN
    nil
  end

  def autoload?(name)
    puts "== WARN: STUB: MNI: Module#autoload?"  unless GS_NOWARN
    false
  end

  # TODO: Module#module_eval: move the implementation of
  # Class#module_eval from Class.rb here.
  def module_eval
    puts "== WARN: STUB: MNI: Module#module_eval"  unless GS_NOWARN
  end

  alias class_eval module_eval

  # class_variables inherited from Behavior

  def clone
    puts "== WARN: STUB: MNI: Module#clone"  unless GS_NOWARN
    nil
  end

  def const_missing(symbol)
    puts "== WARN: STUB: MNI: Module#const_missing"  unless GS_NOWARN
    nil
  end

  def include?(mod)
    puts "== WARN: STUB: MNI: Module#include?"  unless GS_NOWARN
    false
  end

  def included_modules
    puts "== WARN: STUB: MNI: Module#included_modules"  unless GS_NOWARN
    []
  end

  def instance_method(symbol)
    puts "== WARN: STUB: MNI: Module#instance_method"  unless GS_NOWARN
    nil
  end

  def instance_methods(inc_super=true)
    puts "== WARN: STUB: MNI: Module#instance_methods"  unless GS_NOWARN
    []
  end

  def method_defined?(symbol)
    puts "== WARN: STUB: MNI: Module#symbol"  unless GS_NOWARN
    false
  end

  # module_eval is above

  def private_class_method(*symbols)
    puts "== WARN: STUB: MNI: Module#private_class_method"  unless GS_NOWARN
    nil
  end

  def private_instance_mtehods(inc_super=true)
    puts "== WARN: STUB: MNI: Module#private_instance_mtehods"  unless GS_NOWARN
    []
  end

  def private_method_defined?(symbol)
    puts "== WARN: STUB: MNI: Module#private_method_defined?"  unless GS_NOWARN
    false
  end

  def protected_instance_methods(inc_super=true)
    puts "== WARN: STUB: MNI: Module#protected_instance_methods"  unless GS_NOWARN
    []
  end

  def protected_method_defined?(symbol)
    puts "== WARN: STUB: MNI: Module#protected_method_defined?"  unless GS_NOWARN
    false
  end

  def public_class_method(*symbols)
    puts "== WARN: STUB: MNI: Module#public_class_method"  unless GS_NOWARN
    nil
  end

  def public_instance_methods(inc_super=true)
    puts "== WARN: STUB: MNI: Module#public_instance_methods"  unless GS_NOWARN
    []
  end

  def public_method_defined?(symbol)
    puts "== WARN: STUB: MNI: Module#public_method_defined?"  unless GS_NOWARN
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
    puts "== WARN: STUB: MNI: Module#private"  unless GS_NOWARN
    # MNI
  end

  def protected(*symbols)
    puts "== WARN: STUB: MNI: Module#protected"  unless GS_NOWARN
    # MNI
  end

  def public(*symbols)
    puts "== WARN: STUB: MNI: Module#public"  unless GS_NOWARN
    # MNI
  end

  def remove_class_variable(symbol)
    puts "== WARN: STUB: MNI: Module#remove_class_variable"  unless GS_NOWARN
    # MNI
  end

  def remove_method(symbol)
    puts "== WARN: STUB: MNI: Module#remove_method"  unless GS_NOWARN
    # MNI
  end

  def undef_method(symbol)
    puts "== WARN: STUB: MNI: Module#undef_method"  unless GS_NOWARN
    # MNI
  end

end
