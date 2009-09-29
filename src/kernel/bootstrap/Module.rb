class Module

  # See also delta/Module.rb and Behavior.rb

  primitive_nobridge 'superclass' , '_rubySuperclass'  # resolves to impl in .mcz

  # make associations holding constants of receiver invariant
  #   does not affect inherited constants
  primitive_nobridge '_freeze_constants', '_rubyConstantsFreeze'

  primitive_nobridge '_include_module', 'includeRubyModule:'

  primitive_nobridge 'ancestors' , 'rubyAncestors'
  primitive_nobridge 'included_modules' , 'rubyIncludedModules'

  primitive_nobridge 'autoload', 'rubyAutoload:file:'
  primitive_nobridge 'autoload?', 'rubyAutoloadFileFor:'

  primitive_nobridge 'instance_variables', 'rubyInstvarNames'

  primitive_nobridge 'freeze' , 'freezeModule'
  primitive_nobridge 'frozen?' , 'moduleFrozen'

  # comparision methods

  primitive_nobridge '_includes_module', '_rubySubclassOf:'

  def <(other)
    if self.equal?(other)
      false
    elsif other.is_a?(Module)
      if self._includes_module(other)
        true
      elsif other._includes_module(self)
        false
      else
        nil
      end
    else
      raise TypeError , 'in Module#< , argument is not a Module'
      nil
    end
  end

  def <=(other)
    if self.equal?(other)
      true
    elsif other.is_a?(Module)
      if self._includes_module(other)
        true
      elsif other._includes_module(self)
        false
      else
        nil
      end
    else
      raise TypeError , 'in Module#< , argument is not a Module'
      nil
    end
  end

  def >(other)
    if self.equal?(other)
      false
    elsif other.is_a?(Module)
      if self._includes_module(other)
        false
      elsif other._includes_module(self)
        true
      else
        nil
      end
    else
      raise TypeError , 'in Module#> , argument is not a Module'
      nil
    end
  end

  def >=(other)
    if other.equal?(self)
      true
    elsif other.is_a?(Module)
      if self._includes_module(other)
        false
      elsif other._includes_module(self)
        true
      else
        nil
      end
    else
      raise TypeError , 'in Module#>= , argument is not a Module'
      nil
    end
  end

  def <=>(other)
    if self.equal?(other)
      0
    elsif self._includes_module(other)
      -1
    elsif other.is_a?(Module) && other._includes_module(self)
      1
    else
      nil
    end
  end

  def ===(obj)
    # return true if obj is an instance of self or of one of self's descendants
    obj.kind_of?( self)
  end

  # class variables support

  primitive_nobridge 'class_variables', 'rubyClassVarNames'

  primitive_nobridge '_class_var_defined', 'rubyClassVarDefined:'

  def class_variable_defined?(name)
    unless name._isSymbol
      if name._isNumeric
        raise NameError, 'illegal class variable name'
      end
      name = Type.coerce_to(name, String, :to_str)
    end
    _class_var_defined(name)
  end

  primitive_nobridge '_class_var_get', 'rubyClassVarGet:'

  def class_variable_get(name)
    unless name._isSymbol
      if name._isNumeric
        raise NameError, 'illegal class variable name'
      end
      name = Type.coerce_to(name, String, :to_str)
    end
    _class_var_get(name)
  end

  primitive_nobridge '_class_var_set', 'rubyClassVarSet:value:'

  def class_variable_set(name, value)
    unless name._isSymbol
      if name._isNumeric
        raise NameError, 'illegal class variable name'
      end
      name = Type.coerce_to(name, String, :to_str)
    end
    _class_var_set(name, value)
  end

  primitive_nobridge '_class_var_remove', 'rubyClassVarRemove:'

  def remove_class_variable(name)
    unless name._isSymbol
      if name._isNumeric
        raise NameError, 'illegal class variable name'
      end
      name = Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    _class_var_remove(name)
  end

  # constants access methods

  primitive_nobridge 'constants', 'rubyConstants'  # includes superclasses' constants

  def self.constants
    Object.constants
  end

  primitive_nobridge '_const_defined', 'rubyConstDefined:'

  def const_defined?(name)
    # does not look in superclasses (but 1.9 does)
    if name._isSymbol
      sym = name
    else
      str = Type.coerce_to(name, String, :to_str)
      sym = str.to_sym
    end
    res = self._const_defined(sym)
    if res.equal?(false)
      if str.equal?(nil)
        str = name.to_s   # arg is a Symbol
      end
      if str =~ /^[A-Z](\w)*\z/
        return false
      else
        raise NameError, 'arg to const_defined? is not a valid name for a constant'
      end
    end
    res
  end

  primitive_nobridge '_const_get',      'rubyConstAt:'

  def const_get(name)
    unless name._isString or name._isSymbol
      name = Type.coerce_to(name, String, :to_str)
    end
    _const_get(name)
  end

  primitive_nobridge '_const_set',      'rubyConstDecl:put:'

  def const_set(name, value)
    if name._isSymbol
      sym = name
      str = name.to_s
    else
      str = Type.coerce_to(name, String, :to_str)
      sym = nil
    end
    unless str =~ /^[A-Z](\w)*\z/
      raise NameError, 'arg to const_set? is not a valid name for a constant'
    end
    if sym.equal?(nil)
      sym = str.to_sym
    end
    self._const_set(sym, value)
  end

  # Invoked as a callback when a reference to an undefined symbol is made.
  def const_missing(symbol)
    raise NameError, "uninitialized constant #{symbol}"
  end

  # append_features deprecated, not implemented , see Module#included

  def include(*names)
    # this variant gets bridge methods
    names.reverse.each do |name|
      _include_module(name)
    end
  end
  def include(name)
    # variant needed for bootstrap
    _include_module(name)
  end

  #  define_method   is implemented in Behavior

  # Invoked as a callback when a_module includes receiver.
  # supercedes  append_features
  def included(a_module)
  end

  # Callback invoked whenever the receiver is used to extend an object.
  # The object is passed as a paramter.
  def extended(a_module)
  end

  def method_added(a_symbol)
    # invoked from code in .mcz when a method is compiled into receiver
    # overrides the bootstrap implementation in Behavior_ruby.gs
    nil
  end

  def method_removed(a_symbol)
    # invoked from code in .mcz when a method is removed from receiver
    nil
  end

  # Invoked as a callback when a method is undefined in the receiver
  def method_undefined(symbol)
  end

  primitive_nobridge '_module_funct', 'addModuleMethod:'

  def module_function(*names)
    if names.length > 0
      names.each { |name|
        unless name._isSymbol
          name = Type.coerce_to(name, String, :to_str)
          name = name.to_sym
        end
        _module_funct(name)
      }
    else
      _module_funct(true)  # enable the _module_methods_all semantics
    end
    self
  end

  # The @name fixed instVar in Module may hold a Smalltalk class name
  # which may be different from the Ruby name for the Module .
  # The  rubyFullName method obtains the name from the Module's
  # rubyNameSpace if possible, before using @name .
  primitive_nobridge '_fullName', 'rubyFullName'
  def name
    _fullName
  end

  # Module.nesting is compiled to an env0 send of ( aRubyConstantRef nesting ) .
  # Module.nesting is not usable within bootstrap code.
  #
  def self.nesting(*args, &blk)
    # You may also get this error if Module.nesting is used in bootstrap code
    # or if the receiver of nesting is not recognizable at compile time
    # as a reference to the constant Module .
    raise ArgumentError , 'only zero arg form of Module.nesting is supported'
  end

  class_primitive_nobridge 'allocate', 'newModule'

  def self.new(*args, &block)
    m = allocate
    m.initialize(*args)
    if block_given?
      m.module_eval(&block)
    end
    m
  end

  def initialize(*args)
    self
  end

  primitive_nobridge '_set_protection_methods*', 'setProtection:methods:'

  def private(*names)
    # if names empty, set default visibility for subsequent methods to private
    #  and shutoff _module_methods_all
    _set_protection_methods(2, *names)
  end

  def public(*names)
    #  if names empty, set default visibility for subsequent methods to public
    #  and shutoff _module_methods_all
    _set_protection_methods(0, *names)
  end


  def protected(*names)
    # if names empty, set default visibility for subsequent methods to protected
    #  and shutoff _module_methods_all
    _set_protection_methods(1, *names)
  end

  primitive_nobridge '_set_protection_classmethods*', 'setProtection:classmethods:'

  def private_class_method(*symbols)
    _set_protection_classmethods(2, *symbols)
  end

  def public_class_method(*symbols)
    _set_protection_classmethods(0, *symbols)
  end

  primitive_nobridge '_remove_const', 'rubyRemoveConst:'

  def remove_const(name)
    unless name._isSymbol
      name = Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    _remove_const(name)
  end

  # primitive_nobridge '_method_protection', 'rubyMethodProtection' # not used from Ruby

  # If receiver is not already persistable , makes it persistable and 
  # ensures that it is referenced by the parent class/module's persistent name space.
  #
  # Since Modules and Classes are namespaces, all of receivers
  # constants should hold persistable values or an exception will be raised
  # at commit time.
  #
  # If receiver is a class, this method does not affect the persistable
  # instances flag (which is set to true by default). See
  # <tt>Class#maglev_persistable_instances</tt> for controlling whether
  # instances of the class are persistable.
  def maglev_persistable
    self._set_persistable
  end
  primitive_nobridge '_set_persistable', '_setPersistable'

  # Returns true if receiver is marked as persistable; false otherwise.
  primitive_nobridge 'maglev_persistable?', '_persistable'
end
