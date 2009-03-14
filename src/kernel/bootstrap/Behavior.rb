class Behavior
  # Behavior has methods that are shared by meta classes (singletons) and
  # Module and Class.  E.g., attr_* are needed for singletons as well as
  # normal class/modules so they are defined up here.

  primitive 'alias_method', 'rubyAlias:from:'
  primitive_nobridge 'include', 'includeRubyModule:'
  primitive_nobridge '_instVarAt', 'rubyInstvarAt:'
  primitive_nobridge '_instVarAtPut', 'rubyInstvarAt:put:'
  primitive_nobridge 'instance_variables', 'rubyInstvarNames'
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

  primitive_nobridge 'class_variables', 'rubyClassVarNames' 

  # def class_variable_defined?(string) ; end 
  primitive_nobridge 'class_variable_defined?', 'rubyClassVarDefined:'

  # def class_variable_get(string) ; end 
  primitive_nobridge 'class_variable_get', 'rubyClassVarGet:'

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
    _define_method_block(sym, &blk)
  end

  def inspect(touchedSet=nil)
    name
  end

  primitive_nobridge 'instance_method', 'rubyUnboundMethodFor:'
     # one arg, a Symbol

  primitive_nobridge '_method_defined', 'rubyMethodDefined:protection:'

  def method_defined?(symbol)
    _method_defined(symbol, -1)
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

  primitive_nobridge '_method_protection', 'rubyMethodProtection'

  primitive_nobridge '_set_protection_methods*', 'setProtection:methods:'

  def private(*names)
    # if names empty, set default visibility for subsequent methods to private
    _set_protection_methods(2, *names)
  end

  def public(*names)
    #  if names empty, set default visibility for subsequent methods to public
    _set_protection_methods(0, *names)
  end


  def protected(*names)
    #  if names empty, set default visibility for subsequent methods to protected
    _set_protection_methods(1, *names)
  end

  def to_s
    name
  end

  def public_method_defined?(symbol)
    _method_defined(symbol, 0)
  end

  def protected_method_defined?(symbol)
    _method_defined(symbol, 1)
  end

  def private_method_defined?(symbol)
    _method_defined(symbol, 2)
  end

  primitive_nobridge 'remove_method', 'rubyRemoveMethod:'

  #   for now, undef_method is same as remove_method , there is no
  #   other way to prevent a class responding to a selector.
  #
  primitive_nobridge 'undef_method', 'rubyRemoveMethod:'

end
