class Behavior
  # Behavior has methods that are shared by meta classes (singletons) and
  # Module and Class.  E.g., attr_* are needed for singletons as well as
  # normal class/modules so they are defined up here.

  primitive '_alias_method', 'rubyAlias:from:'

  def alias_method(new_name, old_name)
    unless new_name._isSymbol
      new_name = Type.coerce_to(new_name, String, :to_str)
    end
    unless old_name._isSymbol
      old_name = Type.coerce_to(old_name, String, :to_str)
    end
    self._alias_method(new_name, old_name) 
  end

  primitive_nobridge '_module_eval_string', '_moduleEvalString:with:binding:'
  primitive_nobridge '_module_eval&', '_moduleEval:'

  primitive_nobridge 'ancestor_modules_names', 'rubyAncestorModulesNames' 

  # map name to _rubyName so name will work for metaclasses
  primitive 'name' , '_rubyName'

  def _isBehavior
    true
  end

  def attr(name, writeable=false)
    attr_reader(name)
    attr_writer(name) if writeable
  end

  def attr_accessor(*names)
    names.each do |n|
      attr(n, true)
    end
  end

  def _attr_type_check(name)
    if name._isFixnum
      name.id2name
    elsif (name._isString || name._isSymbol)
      name
    else
      Type.coerce_to(name, String, :to_str)
    end
  end

  def attr_reader(*names)
    names.each do |n|
      the_name = self._attr_type_check(n)
      module_eval "def #{the_name}; @#{the_name}; end"
    end
  end

  def attr_writer(*names)
    names.each do |n|
      the_name = self._attr_type_check(n)
      module_eval "def #{the_name}=(v); @#{the_name} = v; end"
    end
  end

  primitive_nobridge '_define_method_meth' , 'defineMethod:method:'
  primitive_nobridge '_define_method_block&' , 'defineMethod:block:'

  def define_method(sym, meth)
    m = meth
    if m.is_a?(Proc)
      m = meth._block
    end
    if m._isBlock
      _define_method_block(sym, &m)
    else
      _define_method_meth(sym, meth)
    end
  end

  def define_method(sym, &blk)
    define_method(sym, blk)
  end

  def inspect
    name
  end

  primitive_nobridge '_instance_method', 'rubyUnboundMethodFor:'

  def instance_method(name)
    unless name._isSymbol
      name = Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    self._instance_method(name)
  end

  primitive_nobridge '_method_defined', 'rubyMethodDefined:protection:'

  def method_defined?(name)
    unless name._isSymbol
      name = Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    _method_defined(name, -1)
  end

  def method_added(a_symbol)
    # invoked from code in .mcz when a method is compiled into receiver
    # overrides the bootstrap implementation in Behavior_ruby.gs 
    # note also implementation of method_added in Module 
    self.singleton_method_added(a_symbol)
  end

  def method_removed(a_symbol)
    # invoked from code in .mcz when a method is removed from receiver
    # note also implementation of method_removed in Module 
    self.singleton_method_removed(a_symbol)
  end

  def module_eval(*args)
    # bridge methods would interfere with VcGlobals logic
    raise ArgumentError, 'expected 1 arg'
  end

  def module_eval(str)
    string = Type.coerce_to(str, String, :to_str)
    ctx = self._binding_ctx(0)
    bnd = Binding.new(ctx, self, nil)
    vcgl = [ self._getRubyVcGlobal(0x20) ,
             self._getRubyVcGlobal(0x21) ]
    res = _module_eval_string(string, vcgl, bnd)
    vcgl[0]._storeRubyVcGlobal(0x20)
    vcgl[1]._storeRubyVcGlobal(0x21)
    res
  end

  def module_eval(&block)
    # no VcGlobal logic here, the block uses $~ of it's home context
    _module_eval(&block)
  end

  def module_eval(str, file=nil, line=nil)
    _stub_warn("Behavior#module_eval: ignoring file and line numbers")
    string = Type.coerce_to(str, String, :to_str)
    ctx = self._binding_ctx(0)
    bnd = Binding.new(ctx, self, nil)
    vcgl = [ self._getRubyVcGlobal(0x20) ,
             self._getRubyVcGlobal(0x21) ]
    res = _module_eval_string(string, vcgl, bnd)
    vcgl[0]._storeRubyVcGlobal(0x20)
    vcgl[1]._storeRubyVcGlobal(0x21)
    res
  end

  alias class_eval module_eval

  def to_s
    name
  end

  def public_method_defined?(name)
    unless name._isSymbol
      name = Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    _method_defined(name, 0)
  end

  def protected_method_defined?(name)
    unless name._isSymbol
      name = Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    _method_defined(name, 1)
  end

  def private_method_defined?(name)
    unless name._isSymbol
      name = Type.coerce_to(name, String, :to_str)
      name = name.to_sym
    end
    _method_defined(name, 2)
  end

  primitive_nobridge 'remove_method', 'rubyRemoveMethod:'

  def _ruby_inherited(a_class)
    # do the actual send  of inherited to self, so Rails can install
    #   a private inherited method.   Trac 428.
    self.inherited(a_class)
  end

  primitive_nobridge 'undef_method', 'rubyUndefMethod:'

end
