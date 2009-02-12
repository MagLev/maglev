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
  primitive_nobridge 'const_get',      'rubyGlobalAt:'
  primitive_nobridge 'const_set',      'rubyConstAt:put:'
  primitive_nobridge 'include',        'includeRubyModule:'

  primitive_nobridge 'autoload', 'rubyAutoload:file:'
  primitive_nobridge 'autoload?', 'rubyAutoloadFileFor:'

  # Invoked as a callback when a reference to an undefined symbol is made.
  def const_missing(symbol)
    raise NameError, "uninitialized constant #{symbol}"
  end

  #  define_method   is implemented in Behavior

  # Invoked as a callback when a method is added to the reciever
  def method_added(symbol)
  end

  # Invoked as a callback when a method is removed from the reciever
  def method_removed(symbol)
  end

  # Invoked as a callback when a method is undefined in the reciever
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

  primitive_nobridge '_fullName', 'rubyFullName'
  def name
    _fullName
  end

  class_primitive_nobridge 'new', 'newModule'

  primitive_nobridge 'remove_const', 'rubyRemoveConst:'

  primitive_nobridge '_method_protection', 'rubyMethodProtection'

end
