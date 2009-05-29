class Module

  # NOTE: Most of the implementation of Module is in delta/Module.rb due to
  # a bootstrapping issue: Most methods are stubbed, and as an aid to
  # debugging, they print out a warning so we can figure out which methods
  # really need work.  BUT, Kernel.puts requires File, which hasn't been
  # loaded yet, so we have to wait until after bootstrap/File.rb is loaded
  # before you can do a puts.  Since all of the stubs in module have a
  # puts, they are temporarily there.  As they are implemented, we should
  # pull them into here.

  primitive_nobridge 'constants',      'rubyConstants'
  primitive_nobridge 'const_defined?', 'rubyConstDefined:'
  primitive_nobridge '_const_get',      'rubyGlobalAt:'
  primitive_nobridge 'const_set',      'rubyConstAt:put:'

  # make associations holding constants of receiver invariant
  #   does not affect inherited constants
  primitive_nobridge '_freeze_constants', '_rubyConstantsFreeze'

  primitive_nobridge '_include_module', 'includeRubyModule:'

  primitive_nobridge 'ancestors' , 'rubyAncestors'
  primitive_nobridge 'included_modules' , 'rubyIncludedModules'

  primitive_nobridge 'autoload', 'rubyAutoload:file:'
  primitive_nobridge 'autoload?', 'rubyAutoloadFileFor:'

  def const_get(name)
    unless name._isString or name._isSymbol
      name = Type.coerce_to(name, String, :to_str)
    end
    _const_get(name)
  end

  # Invoked as a callback when a reference to an undefined symbol is made.
  def const_missing(symbol)
    raise NameError, "uninitialized constant #{symbol}"
  end

  # Invokes Module.append_features on each parameter (in reverse order).
  # def include(*names) ; end  #  inherited from Behavior

  #  define_method   is implemented in Behavior

  # Invoked as a callback when a_module includes receiver
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
      names.each{|name|
        unless name.equal?(nil)
          _module_funct(name)
        end
      }
    else
      _module_funct(nil)  # enable the _module_methods_all semantics
    end
  end

  # The @name fixed instVar in Module may hold a Smalltalk class name
  # which may be different from the Ruby name for the Module .
  # The  rubyFullName method obtains the name from the Module's
  # rubyNameSpace if possible, before using @name .
  primitive_nobridge '_fullName', 'rubyFullName'
  def name
    _fullName
  end

  class_primitive_nobridge '_new_module', 'newModule'

  def self.new(*args, &block)
    m = _new_module
    m.initialize(*args)
    if block_given?
      m.module_eval(&block)
    end
    m
  end

  def initialize(*args)
    self
  end

  primitive_nobridge 'remove_const', 'rubyRemoveConst:'

  primitive_nobridge '_method_protection', 'rubyMethodProtection'

end
