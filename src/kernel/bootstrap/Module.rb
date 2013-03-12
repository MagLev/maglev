class Module

  # See also delta/Module.rb

  primitive_nobridge '__instvar_get', 'rubyInstvarAt:'

  def __isBehavior
    true
  end

  class_primitive_nobridge 'allocate', 'newModule'

  def self.new(*args, &block)
    m = allocate
    m.initialize(*args,&block)
    if block_given?
      m.module_eval(&block)
    end
    m
  end

  def initialize(*args)
    self
  end

  primitive_nobridge '__check_include', '_checkIncludeRubyModule:'
  primitive_nobridge '__include_module', '_includeRubyModule:'
  primitive_nobridge '__is_virtual', 'isVirtual'

  # append_features deprecated, but needed by Rails3
  def append_features(other)
    if other.__check_include(self)
      other.__include_module(self)
    end
    self
  end

  def include(*modules)
    # this variant gets bridge methods
    modules.reverse.each do |a_module|
      a_module.append_features(self)
      if a_module.respond_to? :included
        a_module.included(self)
      end
    end
    self
  end
  def include(a_module)
    # variant needed for bootstrap, eventual version in Module3.rb
    a_module.append_features(self)
    self
  end

  # Invoked as a callback when a_module includes receiver.
  def included(a_module)
  end

  # Callback invoked whenever the receiver is used to extend an object.
  # The object is passed as a paramter.
  def extended(a_module)
  end

  # --------- remainder of methods approximately alphabetical

  primitive '__alias_method', 'rubyAlias:from:'

  def alias_method(new_name, old_name)
    unless new_name._isSymbol
      new_name = Maglev::Type.coerce_to(new_name, String, :to_str)
    end
    unless old_name._isSymbol
      old_name = Maglev::Type.coerce_to(old_name, String, :to_str)
    end
    self.__alias_method(new_name, old_name)
  end

  # def __filter_method_names(identity_set) ; end
  #   arg is an IdentitySet of Symbols,
  #   result is an Array of Strings, excluding  those beginning with '__'
  class_primitive_nobridge '__filter_method_names', 'excludeInternalMethodNames:'

  primitive_nobridge 'ancestor_modules_names', 'rubyAncestorModulesNames'

  primitive_nobridge 'ancestors' , 'rubyAncestors'

  def attr(name, writeable=false)
    attr_reader(name)
    attr_writer(name) if writeable
  end

  def attr_accessor(*names)
    names.each do |n|
      attr(n, true)
    end
  end

  def __attr_type_check(name)
    if name._isFixnum
      name.id2name
    elsif (name._isString || name._isSymbol)
      name
    else
      Maglev::Type.coerce_to(name, String, :to_str)
    end
  end

  # attr_reader, attr_writer in Module3.rb

  primitive_nobridge 'autoload', 'rubyAutoload:file:'
  primitive_nobridge 'autoload?', 'rubyAutoloadFileFor:'

  # class variables support

  primitive_nobridge 'class_variables', 'rubyClassVarNames'

  primitive_nobridge '__class_var_defined', 'rubyClassVarDefined:'

  def class_variable_defined?(name)
    unless name._isSymbol
      if name._isNumeric
        raise NameError, 'illegal class variable name'
      end
      name = Maglev::Type.coerce_to(name, String, :to_str)
    end
    __class_var_defined(name)
  end

  primitive_nobridge '__class_var_get', 'rubyClassVarGet:'

  def class_variable_get(name)
    unless name._isSymbol
      if name._isNumeric
        raise NameError, 'illegal class variable name'
      end
      name = Maglev::Type.coerce_to(name, String, :to_str)
    end
    __class_var_get(name)
  end

  primitive_nobridge '__class_var_set', 'rubyClassVarSet:value:'

  def class_variable_set(name, value)
    unless name._isSymbol
      if name._isNumeric
        raise NameError, 'illegal class variable name'
      end
      name = Maglev::Type.coerce_to(name, String, :to_str)
    end
    __class_var_set(name, value)
  end

  # constants access methods

  primitive_nobridge '_constants', 'rubyConstants:'  # includes superclasses' constants

  def constants(inherit = true)
    _constants(inherit)
  end

  def self.constants
    Object.constants
  end

  primitive_nobridge '__const_defined', 'rubyConstDefined:'

  def const_defined?(name)
    # does not look in superclasses (but 1.9 does)
    if name._isSymbol
      sym = name
    else
      str = Maglev::Type.coerce_to(name, String, :to_str)
      sym = str.to_sym
    end
    res = self.__const_defined(sym)
    if res._equal?(false)
      if str._equal?(nil)
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

  primitive_nobridge '__const_get',      'rubyConstAt:'

  def const_get(name)
    unless name._isString or name._isSymbol
      name = Maglev::Type.coerce_to(name, String, :to_str)
    end
    __const_get(name)
  end

  primitive_nobridge '__const_set',      'rubyConstDecl:put:'

  def const_set(name, value)
    if name._isSymbol
      sym = name
      str = name.to_s
    else
      str = Maglev::Type.coerce_to(name, String, :to_str)
      sym = nil
    end
    unless str =~ /^[A-Z](\w)*\z/
      raise NameError, 'arg to const_set? is not a valid name for a constant'
    end
    if sym._equal?(nil)
      sym = str.to_sym
    end
    self.__const_set(sym, value)
  end

  # Invoked as a callback when a reference to an undefined symbol is made.
  def const_missing(symbol)
    # final implementation in delta/Module.rb , this implementation avoids MNU
    #   while constructing the exception
    raise NameError, 'uninitialized constant during bootstrap'
  end

  primitive_nobridge '__define_method_meth' , 'defineMethod:method:'
  primitive_nobridge '__define_method_block&' , 'defineMethod:block:'

  def define_method(sym, meth)
    sym = Maglev::Type.coerce_to(sym, Symbol, :to_sym)
    m = meth
    if meth._is_a?(Proc)
      m = meth.__block
    end
    if m._isBlock
      __define_method_block(sym, &m)  # result is a UnboundMethod
      if (meth._is_a?(Proc))  # fix bad return type discovered during Trac 785
        res = meth
      else
        res = Proc.new(&m)
      end
    else
      res = __define_method_meth(sym, meth)
    end
    self.method_added(sym)
    res
  end

  def define_method(sym, &block)
    define_method(sym, block)
  end

  def dup
    raise NotImplementedError, "Module#dup"
  end

  # make associations holding constants of receiver invariant
  #   does not affect inherited constants
  primitive_nobridge '__freeze_constants', '_rubyConstantsFreeze'

  primitive_nobridge 'freeze' , 'freezeModule'
  primitive_nobridge 'frozen?' , 'moduleFrozen'

  primitive_nobridge 'included_modules' , 'rubyIncludedModules'

  primitive_nobridge '__includes_module', '_rubySubclassOf:'

  primitive_nobridge '__instance_method', 'rubyUnboundMethodFor:'
  primitive_nobridge '__gs_method', 'rubyMethodFor:instanceMethod:'

  def instance_method(name)
    unless name._isSymbol
      name = Maglev::Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    self.__instance_method(name)
  end

  primitive_nobridge 'instance_variables', 'rubyInstvarNames'

  primitive 'instance_variable_defined?' , 'rubyIvDefined:'

  primitive 'inspect', '_rubyInspect'

  primitive_nobridge '__method_defined', 'rubyMethodDefined:protection:'

  def method_defined?(name)
    unless name._isSymbol
      name = Maglev::Type.coerce_to(name, String, :to_str)
      name = Symbol.__existing_symbol(name)
      if name.equal?(nil)
        return false
      end
    end
    __method_defined(name, 0)
  end

  def module_eval(*args, &block_arg)
    # save current protection level to restore afterwards; set public
    protection_level = _method_protection
    public
    # should always come here via a bridge method , thus 0x3N for vcgl ...
    nargs = args.size
    if nargs < 1
      if block_arg._not_equal?(nil)
        module_eval_value = __module_eval(nil, block_arg)
        __set_protection_methods(protection_level)
        return module_eval_value
      end
        __set_protection_methods(protection_level)
      raise ArgumentError, 'too few args'
    end
    if nargs > 3
        __set_protection_methods(protection_level)
      raise ArgumentError, 'too many args'
    end
    # no ArgumentError for both string and explicit block args yet ;
    #  passing implicit block_arg if no explicit block arg, so it can
    #  be put in the binding...
    lex_path = self.__getRubyVcGlobal(0x32) # __lexPath, synthesized by AST to IR code in .mcz
    str = args[0]
    string = Maglev::Type.coerce_to(str, String, :to_str)
    ctx = self.__binding_ctx(1)
    bnd = Binding.new(ctx, self, block_arg)
    bnd.__set_lex_scope(lex_path)
    vcgl = [ self.__getRubyVcGlobal(0x30) ,
             self.__getRubyVcGlobal(0x31) ]
    bblk = bnd.block
    unless bblk._equal?(nil)
      vcgl << bblk
    end
    m_args = [ bnd,
               args[1], # file
               args[2] ]  # line
    res = __module_eval_string(string, vcgl, *m_args )
    vcgl[0].__storeRubyVcGlobal(0x30)
    vcgl[1].__storeRubyVcGlobal(0x31)
    __set_protection_methods(protection_level)
    res
  end

  primitive_nobridge_env '__module_eval', '_moduleEval', ':block:'
    # lex_path ignored, there is no source string to compile
    # no VcGlobal logic here, the block uses $~ of it's home context

  alias class_eval module_eval


  def public_method_defined?(name)
    unless name._isSymbol
      name = Maglev::Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    __method_defined(name, 0)
  end

  def protected_method_defined?(name)
    unless name._isSymbol
      name = Maglev::Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    __method_defined(name, 1)
  end

  def private_method_defined?(name)
    unless name._isSymbol
      name = Maglev::Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    __method_defined(name, 2)
  end

  primitive_nobridge '__remove_method', 'rubyRemoveMethod:'

  def remove_method(sym)
    sym = Maglev::Type.coerce_to(sym, Symbol, :to_sym)
    self.__remove_method(sym)
  end

  def __ruby_inherited(a_class)
    # do the actual send  of inherited to self, so Rails can install
    #   a private inherited method.   Trac 428.
    self.inherited(a_class)
  end

  primitive_nobridge 'superclass' , '_rubySuperclass'  # resolves to impl in .mcz

  primitive_nobridge '__undef_method', 'rubyUndefMethod:'
  def undef_method(*names)
    names.each { |name| __undef_method(name) }
  end
  def undef_method(name)
    __undef_method(name)
  end

  primitive_nobridge '__class_var_remove', 'rubyClassVarRemove:'

  def remove_class_variable(name)
    unless name._isSymbol
      if name._isNumeric
        raise NameError, 'illegal class variable name'
      end
      name = Maglev::Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    __class_var_remove(name)
  end

  # Returns true if receiver is marked as persistable; false otherwise.
  primitive_nobridge 'maglev_persistable?', '_persistable'

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
  def maglev_persistable(methodsPersistable = false)
    methodsPersistable = (methodsPersistable == true)
    self.__set_persistable(methodsPersistable)
  end

  primitive_nobridge '__set_persistable', '_setPersistable:'

  # Redefine a class and migrate it's instances. Will abort or commit
  # the current transaction.
  #
  # Currently accepts the following optional hash parameters
  #   :commit => Boolean   # Whether to commit the current transaction or not
  #                        # Defaults to false in transient, true in persistent mode
  #   :new_name => String  # The intended new class name and namespace
  #                        # Defaults to the current name and namespace
  def maglev_redefine(params = {})
    unless (params[:commit] ||= Maglev.persistent?)
      raise Exception, "maglev_redefine requires a transaction abort, however, an abort would result in lost data" if Maglev::System.needs_commit
      Maglev.abort_transaction
    end

    new_name = (params[:new_name] || name).to_s.split("::")[-1]
    old_ns = name.split("::")[0...-1].inject(Object) {|c,n| c.const_get(n)}
    new_ns = new_name.split("::")[0...-1].inject(Object) {|c,n| cls.const_get(n)}

    Maglev.persistent { old_ns.remove_const(name.split("::")[-1]) }
    yield
    Maglev.commit_transaction if params[:commit]

    if new_ns.const_defined?(new_name)
      self.__migrate_instances_to(new_ns.const_get(new_name))
    else
      raise ArgumentError, "The block passed to #maglev_redefine failed to create a class named #{new_name} under #{new_ns}"
    end
    Maglev.commit_transaction if params[:commit]
    new_ns.const_get(new_name)
  end
  primitive '__migrate_instances_to', 'migrateInstancesTo:'

  # Invoked as a callback when a method is added to the receiver
  def method_added(a_symbol)
    nil
  end

  # Invoked as a callback when a method is removed from the receiver
  def method_removed(a_symbol)
    nil
  end

  # Invoked as a callback when a method is undefined in the receiver
  def method_undefined(symbol)
    nil
  end

  primitive     '__module_eval_string*', '_moduleEvalString:with:args:'

  primitive_nobridge '__module_funct', 'addModuleMethod:'

  def module_function(*names)
    if names.length > 0
      names.each { |name|
        unless name._isSymbol
          name = Maglev::Type.coerce_to(name, String, :to_str)
          name = name.to_sym
        end
        __module_funct(name)
      }
    else
      __module_funct(true)  # enable the _module_methods_all semantics
    end
    self
  end

  # The @_st_name fixed instVar in Module may hold a Smalltalk class name
  # which may be different from the Ruby name for the Module .
  # The  rubyFullName method obtains the name from the Module's
  # rubyNameSpace if possible, before using @_st_name .
  primitive 'name', 'rubyFullName'

  # Module.nesting is compiled to an env0 send of ( aRubyConstantRef nesting ) .
  # Module.nesting is not usable within bootstrap code.
  #

  # Return an Array of the Modules nested at the point of call.
  # def self.nesting; end
  class_primitive 'nesting', 'moduleNesting'

  primitive_nobridge '_method_protection', 'rubyMethodProtection'

  PROTECTION_PUBLIC = 0
  PROTECTION_PROTECTED = 1
  PROTECTION_PRIVATE = 2

  def protection
    _method_protection
  end

  primitive_nobridge '__set_protection_methods*', 'setProtection:methods:'

  def private(*names)
    # if names empty, set default visibility for subsequent methods to private
    #  and shutoff _module_methods_all
    __set_protection_methods(PROTECTION_PRIVATE, *names)
  end

  def public(*names)
    #  if names empty, set default visibility for subsequent methods to public
    #  and shutoff _module_methods_all
    __set_protection_methods(PROTECTION_PUBLIC, *names)
  end


  def protected(*names)
    # if names empty, set default visibility for subsequent methods to protected
    #  and shutoff _module_methods_all
    __set_protection_methods(PROTECTION_PROTECTED, *names)
  end

  primitive_nobridge '__set_protection_classmethods*', 'setProtection:classmethods:'

  def private_class_method(*symbols)
    __set_protection_classmethods(2, *symbols)
  end

  def public_class_method(*symbols)
    __set_protection_classmethods(0, *symbols)
  end

  primitive_nobridge '__remove_const', 'rubyRemoveConst:'

  primitive_nobridge '__remove_iv', 'rubyRemoveIv:'

  # remove_instance_variable inherited from object

  def remove_const(name)
    unless name._isSymbol
      name = Maglev::Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    __remove_const(name)
  end

  primitive 'to_s' , '_rubyInspect'

  # def transient_const_set(name, &block) in delta/Module.rb

  # comparision methods

  def <(other)
    if self._equal?(other)
      false
    elsif other._is_a?(Module)
      if self.__includes_module(other)
        true
      elsif other.__includes_module(self)
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
    if self._equal?(other)
      true
    elsif other._is_a?(Module)
      if self.__includes_module(other)
        true
      elsif other.__includes_module(self)
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
    if self._equal?(other)
      false
    elsif other._is_a?(Module)
      if self.__includes_module(other)
        false
      elsif other.__includes_module(self)
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
    if other._equal?(self)
      true
    elsif other._is_a?(Module)
      if self.__includes_module(other)
        false
      elsif other.__includes_module(self)
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
    if self._equal?(other)
      0
    elsif self.__includes_module(other)
      -1
    elsif other._is_a?(Module) && other.__includes_module(self)
      1
    else
      nil
    end
  end

  def ===(obj)
    # return true if obj is an instance of self or of one of self's descendants
    obj._kind_of?( self)
  end

  def private_constant(*args)
    warn "NotImplemented: Module#private_constant"
  end

end
