class Object
  include Kernel

  # Begin private helper methods

  #  primitive_nobridge has max of 3 normal args, 1 star arg, 1 block arg
  #  example with a block argument
  #    primitive_nobridge '_foo*&' , '_foo:a:b:c:d:e:'
  #  example without a block argument
  #    primitive_nobridge '_foo*' , '_foo:a:b:c:d:'

  # class is installed by RubyContext>>installPrimitiveBootstrap: , but we
  #  install again here to get bridge methods
  primitive_nobridge 'class', 'class'
  primitive_nobridge '__class', 'class'

  primitive_nobridge '__singleton_class', 'rubySingletonClass'
  primitive_nobridge '__class_for_extend', 'rubySingletonClassForExtend'

  #  Reimplementation of the following special selectors is disallowed
  #  by the parser and they are optimized by code generator to special bytecodes.
  #  Attempts to reimplement them will cause compile-time errors.
  # _isArray    # smalltalk   _isArray
  # _isBlock    # smalltalk   _isExecBlock
  # _isFixnum   # smalltalk   _isSmallInteger
  # _isFloat    # smalltalk   _isFloat
  # _isHash     # smalltalk   _isRubyHash
  # _isInteger  # smalltalk  _isInteger
  # _isNumeric  # smalltalk   _isNumber
  # _isRegexp   # smalltalk   _isRegexp
  # _isRange    # smalltalk   _isRange
  # _isString   # smalltalk   _isRubyString  # returns false for Symbols
  # _isSymbol   # smalltalk   _isSymbol      # returns false for Strings
  # _isStringOrSymbol # smalltalk _isOneByteString


  #   __isSpecial is used by marshal.rb . It is not a special selector
  primitive_nobridge '__isSpecial', 'isSpecial'

  def __isBehavior
    false
  end

  #  Private method __each contains on:do: handler for RubyBreakException ,
  #  all env1 sends of each& are compiled as sends of __each&
  #  Attempts to reimplement __each& will fail with a compile error.
  primitive_nobridge_env '__each&',   '_rubyEach' , ':'

  # __storeRubyVcGlobal is used by methods that need to store into
  #   caller(s) definition(if any) of $~ or $_  .
  #  Receiver is value to be stored.
  #  See smalltalk code in Object for documentation.
  primitive_nobridge '__storeRubyVcGlobal' , '_storeRubyVcGlobal:'
  #  __getRubyVcGlobal returns caller(s) value of $~ or $_ , or nil
  primitive_nobridge '__getRubyVcGlobal' , '_getRubyVcGlobal:'

  # argument to _bindingContext: is number of frames up from sender
  #   of Object.binding from which to create the binding .
  #  Parser has to know about methods which may send _binding_ctx,
  #  to ensure such methods are created with a VariableContext.
  #  see setSendsBinding and the *BindingNode , *EvalNode classes in .mcz
  primitive_nobridge '__binding_ctx' , '_bindingContext:'

  # Special semantics for send of  super  while in bootstrap:
  #   During bootstrap, a send of super with no args specified
  #   passes an implicit block argument(the declared block parameter or nil),
  #   otherwise only the exact args to super are passed.
  #
  #   Outside of bootstrap,  super alway passes an explicit or implicit
  #   block argument.  See RubySuperNode in .mcz and Trac 454,634 .

  # Special semantics of    ensure      while in bootstrap:
  #   During bootstrap,  a ruby  ensure   translates to the
  #   Smalltalk ExecBlock>>ensure:  semantics, and the
  #   ensure block will be executed after any rescue blocks run.
  #   Outside of bootstrap,   ensure translates to
  #   ExecBlock>>rubyEnsure:  and the ensure block executes before
  #   rescue blocks higher in the stack are run.
  #   Use of rubyEnsure:  fixes Trac 720 .
  #   Use of ensure: in bootstrap code makes the ensure faster .

  # End private helper methods

  primitive_nobridge '==', '='
  primitive_nobridge '__eql?', '='
  primitive 'hash'
  primitive 'object_id', 'asOop'
  primitive '__id__' , 'asOop'  # included in public names query results
  primitive '__id' , 'asOop'
  primitive '__identity_hash', 'identityHash'

  primitive_nobridge '__name_for_mnu', '_nameForMethodMissing'

  primitive 'nil?' , '_rubyNilQ'

  # compiler translates ! and  not   tokens to  _not
  def not
    self._not # _not is a special send compiled direct to a bytecode
  end

  # rubySend: methods implemented in svn/image/ruby/pre_prim_methods.gs
  primitive_nobridge_env 'send',  '__rubySend', ':'
  primitive_nobridge_env 'send',  '__rubySend', ':with:'
  primitive_nobridge_env 'send',  '__rubySend', ':with:with:'
  primitive_nobridge_env 'send',  '__rubySend', ':with:with:with:'
  primitive_nobridge_env 'send&', '__rubySend', ':block:'
  primitive_nobridge_env 'send&', '__rubySend', ':with:block:'
  primitive_nobridge_env 'send&', '__rubySend', ':with:with:block:'
  primitive_nobridge_env 'send&', '__rubySend', ':with:with:with:block:'
  primitive_nobridge_env 'send*', '__rubySend', ':withArgs:'
  primitive_env          'send*&' , '__rubySend', ':withArgs:block:'

  # 'send#1*&' , '__send__#1*&' special cased in  bridgeForPrimAllowed: ,
  #  to have bridges during bootstrap.
  #  any other   def send;...  end   gets no bridges  during bootstrap
  #  to allow reimplementation of  send  for methods updating $~ , $_

  #  __send__ defined per MRI, non-overrideable version of send
  #  redefinition of __send__  disallowed by parser after bootstrap finished.
  primitive_nobridge_env '__send__',  '__rubySend', ':'
  primitive_nobridge_env '__send__',  '__rubySend', ':with:'
  primitive_nobridge_env '__send__',  '__rubySend', ':with:with:'
  primitive_nobridge_env '__send__',  '__rubySend', ':with:with:with:'
  primitive_nobridge_env '__send__&', '__rubySend', ':block:'
  primitive_nobridge_env '__send__&', '__rubySend', ':with:block:'
  primitive_nobridge_env '__send__&', '__rubySend', ':with:with:block:'
  primitive_nobridge_env '__send__&', '__rubySend', ':with:with:with:block:'
  primitive_nobridge_env '__send__*', '__rubySend', ':withArgs:'
  primitive_env          '__send__*&', '__rubySend', ':withArgs:block:'

  # redefinition of __perform___ disallowed by parser after bootstrap finished.
  # __perform___  requires next to last arg to be a Symbol with proper suffix
  #   for the number of with: keywords
  #   and last arg is envId
  # __perform are used by RubyParser and FFI::StructLayout
  primitive_nobridge '__perform_se', 'with:perform:env:'
  primitive_nobridge '__perform__', 'perform:env:'
  primitive_nobridge '__perform__se', 'with:with:perform:env:'  # IR generator allows up to 5 colon args before the * arg .

  # redefinition of __perform_method disallowed after bootstrap,
  #  it is used by implementation of eval
  primitive_nobridge '__perform_meth', 'performMethod:'

  def __perfrm_call(*args)
    # invoked only from Object>>_doesNotUnderstand:args:envId:reason:
    self.call(*args)
  end

  def public_send(sym, *args)
    # TODO: public_methods should return an array of symbols
    if protected_methods.include?(sym.to_s) || private_methods.include?(sym.to_s)
      raise NoMethodError(sym)
    else
      send(sym, *args)
    end
  end

  # primitive   '__basic_dup', '_rubyBasicDup'  # use non-singleton class
  primitive   '__basic_clone', 'shallowCopy' # use singleton class

  def clone
    res = self.__basic_clone
    res.initialize_copy(self)
    if self.frozen?
      res.freeze
    end
    res
  end

  primitive_nobridge '__basic_initialize_copy', '_rubyInitializeFrom:'

  # define_singleton_method is 1.9 only

  def dup
    # must call allocate for C extensions to work, can't use __basic_dup
    res = self.class.allocate  # use non-singleton class
    res.__basic_initialize_copy(self)
    res.initialize_copy(self)
    res
  end

  def enum_for(sym = :each , *args)  # added in 1.8.7
    Enumerable::ObjectEnumerator.new(self, sym)
  end

  def to_enum(sym = :each, *args)
    enum_for(sym, *args)
  end

  primitive 'freeze', 'immediateInvariant'
  primitive 'frozen?', 'isInvariant'

  # _set_nostubbing prevents stubbing ram oops to objectIds in in-memory
  #  instance variables that reference committed objects .  should only
  # be used in limited cases when initializing transient state .
  primitive '__set_nostubbing', '_setNoStubbing'

  def kind_of?(amodule)
    self._kind_of?(amodule)  # _kind_of? is a special send compiled direct to a bytecode
  end

  def is_a?(amodule)
    self._is_a?(amodule) # _is_a? is a special send compiled direct to a bytecode
  end

  primitive_nobridge '__responds_to', '_respondsTo:private:flags:'
     # _responds_to flags bit masks are
     #     environmentId                   0xFF
     #     ruby lookup semantics          0x100
     #     ruby receiver is self         0x1000 (for future use)
     #     cache successes in code_gen  0x10000

  def respond_to?(symbol, include_private=false)
    # variant with bridges
    __responds_to(symbol, include_private, 0x10101)
  end

  def respond_to?(symbol )
    # optimize most common form to replace bridge method
    __responds_to(symbol, false, 0x10101)
  end

  def __splat_lasgn_value
    # runtime support for rhs of   x = *y , invoked from generated code
    a = self
    unless a._isArray
      if a._equal?(nil)
        return a
      end
      a = a.__splat_lasgn_value_coerce
    end
    if a._isArray
      sz = a.length
      if sz < 2
        if sz._equal?(0)
          return nil
        else
          return a[0]
        end
      end
    end
    a
  end

  # This is called to coerce argument to an array for splat args, e.g.,
  #   foo.bar(*x)  # will call x.__splat_lasgn_value_coerce
  def __splat_lasgn_value_coerce
    v = begin
      self.to_ary
    rescue
      begin
        self.to_a
      rescue
        self # ignore if not responding to to_ary or to_a
      end
    end
    if v._not_equal?(self)
      unless v._isArray
        raise TypeError, 'arg to splat responded to to_a or to_ary but did not return an Array'
      end
    end
    v
  end

  def __splat_arg_value
    a = self
    unless a._isArray
      a = a.__splat_lasgn_value_coerce
      unless a._isArray
        a = [ self ]
      end
    end
    Array.new(a)
  end

  def __splat_return_value
    # runtime support for  return *v  , invoked from generated code
    v = Maglev::Type.coerce_to_or_nil(self, Array, :to_ary)
    if v._equal?(nil)
      v = Maglev::Type.coerce_to_or_nil(self, Array, :to_a)
      if v._equal?(nil)
        v = self
      end
    end
    sz = v.length
    if sz < 2
      if sz._equal?(0)
        return nil
      else
        return v[0]
      end
    else
      return v
    end
  end

  def __par_asgn_to_ary
    # runtime support for parallel assignment, invoked from generated code
    if self._isArray
      return self
    elsif self._equal?(nil)
      return [ nil ]
    elsif self.respond_to?(:to_ary)
      return self.to_ary
    else
      return [ self ]
    end
  end

  def __par_asgn_star_to_ary
    # runtime support for parallel assignment, invoked from generated code
    if self._isArray
      return self
    elsif self._equal?(nil)
      return [ nil ]
    elsif self.respond_to?(:to_ary)
      return self.to_ary
    elsif self.respond_to?(:to_a)
      return self.to_a
    else
      return [ self ]
    end
  end

  primitive 'print_line', 'rubyPrint:'

  # pause is not standard Ruby, for debugging only .
  #  trappable only by an Exception specifying exactly error 6001
  primitive 'pause', 'pause'

  primitive   '__inspect', '_rubyInspect'

  def inspect
    # sender of _inspect must be in env 1
    self.__inspect
  end

  primitive_nobridge '__instvar_get', 'rubyInstvarAt:'
  primitive_nobridge '__instvar_put', 'rubyInstvarAt:put:'
  primitive_nobridge 'instance_variables', 'rubyInstvarNames'

  primitive 'instance_variable_defined?' , 'rubyIvDefined:'

  def instance_variable_get(a_name)
    unless a_name._isStringOrSymbol
      a_name = Maglev::Type.coerce_to(a_name, String, :to_str)
    end
    __instvar_get(a_name.to_sym)
  end

  def instance_variable_set(a_name, a_val)
    unless a_name._isStringOrSymbol
      a_name = Maglev::Type.coerce_to(a_name, String, :to_str)
    end
    __instvar_put(a_name.to_sym, a_val)
    a_val
  end

  primitive_nobridge 'method', 'rubyMethod:'

  def ===(obj)
      self == obj
  end

  def =~(other)
    false
  end

  # block_given?  is implemented by the ruby parser .
  # implementation in Kernel2.rb handles  sends
  # Attempts to reimplement  block_given? outside of bootstrap code
  #    will fail with a compile error.

  def equal?(anobject)
    self._equal?(anobject) # _equal? is a special send compiled direct to a bytecode
  end

  # _not_equal? is implemented by the ruby parser and optimized to
  #  a special bytecode by the code generator.
  # Attempts to reimplement _not_equal? will fail with a compile error.

  def eql?(other)
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added # fix #951
      ts.remove(self)
      return self.__eql?(other)
    end
    return self == other
  ensure
    ts.remove(self)
  end

  def extend(*modules)
    if (modules.length > 0)
      if self.__isSpecial
        if self._equal?(nil)
          cl = NilClass
        elsif self._equal?(true)
          cl = TrueClass
        elsif self._equal?(false)
          cl = FalseClass
        else
          raise ArgumentError, "cannot extend a special object"
        end
      else
        cl = self.__class_for_extend
      end
      modules.each do |a_module|
        cl.__include_module(a_module)
        if a_module.respond_to? :extended
          a_module.extended(self)
        end
      end
    end
    self
  end

  def initialize(*args, &block)
     self
  end
  # implement common variants to avoid runtime cost of bridge methods
  def initialize
    self
  end
  def initialize(a)
    self
  end
  def initialize(a,b)
    self
  end
  def initialize(a,b,c)
    self
  end
  def initialize(&block)
    self
  end
  def initialize(a, &block)
    self
  end
  def initialize(a,b, &block)
    self
  end
  def initialize(a,b,c, &block)
    self
  end

  def initialize_copy(other)
    # dup and clone are complete, and C extensions not supported yet,
    #   so do nothing
    self
  end

  def initialize_dup(other)
    initialize_copy(other)
  end

  def initialize_clone(other)
    initialize_copy(other)
  end

  def instance_of?(cls)
    # Modified from RUBINIUS
    if self.class._equal?(cls)
      true
    else
      unless cls.__isBehavior
        raise TypeError, 'expected a Class or Module'
      end
      false
    end
  end

  primitive_nobridge '__instance_eval_string*', 'instanceEvalString:with:args:'

  def instance_eval(*args, &block_arg)
    #   should always come here via a bridge method , thus 0x3N for vcgl ...
    nargs = args.size
    if nargs < 1
      if block_arg._not_equal?(nil)
        return __instance_eval(nil, block_arg)
      end
      raise ArgumentError, 'too few args'
    end
    if nargs > 3
      raise ArgumentError, 'too many args'
    end
    # no ArgumentError for both string and explicit block args yet ;
    #  passing implicit block_arg if no explicit block arg, so it can
    #  be put in the binding...
    lex_path = self.__getRubyVcGlobal(0x32) # the __lexPath, synthesized by AST to IR code in .mcz
    str = args[0]
    string = Maglev::Type.coerce_to(str, String, :to_str)
    ctx = self.__binding_ctx(1)
    bnd = Binding.new(ctx, self, block_arg)
    bnd.__set_lex_scope(lex_path)
    vcgl = [ self.__getRubyVcGlobal(0x30), self.__getRubyVcGlobal(0x31) ,
             nil ,  # slot used later on for theSelf
             block_arg
           ]
    m_args = [ bnd,
                args[1], # file
                args[2] ]  # line
    res = __instance_eval_string(string, vcgl, *m_args )
    vcgl[0].__storeRubyVcGlobal(0x30)
    vcgl[1].__storeRubyVcGlobal(0x31)
    res
  end

  primitive_nobridge_env '__instance_eval', 'rubyEval', ':block:'
    # lex_path ignored, there is no source string to compile
    # no VcGlobal logic here, the block uses $~ of it's home context

  def __evalCaller(args, gsnmeth)
    $~ = args[0]
    $_ = args[1]
    rcvr = args[2]
    begin
      res = rcvr.__perform_meth(gsnmeth)
    ensure
      args[0] = $~
      args[1] = $_
    end
    res
  end

  def self.__evalCaller(args, gsnmeth)
    $~ = args[0]
    $_ = args[1]
    rcvr = args[2]
    begin
      res = rcvr.__perform_meth(gsnmeth)
    ensure
      args[0] = $~
      args[1] = $_
    end
    res
  end

  def instance_exec(*args, &block) # added for 1.8.7
    unless block_given?
      raise LocalJumpError, 'no block given'
    end
    blk = block.__set_self(self)
    blk.call(*args)
  end

  primitive_nobridge '__ruby_singleton_methods', 'rubySingletonMethods:protection:'

  primitive_nobridge '__remove_iv', 'rubyRemoveIv:'

  def remove_instance_variable(name)
    unless name._isStringOrSymbol
      name = Maglev::Type.coerce_to(name, String, :to_str)
    end
    if name.__at(0).not_eql?( ?@ )
      raise NameError, "intance variable names must begin with '@'"
    end
    self.__remove_iv(name)
  end

  def singleton_methods(inc_modules = true)
    inc_mods = inc_modules ? true : false
    Module.__filter_method_names(__ruby_singleton_methods(inc_mods, 0))
  end

  primitive_nobridge '__ruby_methods', 'rubyMethods:protection:'

  # If regular is true, retuns an array of the names of methods publicly
  # accessible in receiver and receiver's ancestors.  Otherwise, returns
  # an array of the names of receiver's singleton methods.
  def methods(regular = true)
    if regular
      set = self.__ruby_methods(true, -1) # incl protected meths
    else
      set = self.__ruby_singleton_methods(false, -1) # incl protected meths
    end
    Module.__filter_method_names(set)
  end

  def private_methods(include_super=true)
    Module.__filter_method_names(self.__ruby_methods(include_super, 2))
  end

  def protected_methods(include_super=true)
    Module.__filter_method_names(self.__ruby_methods(include_super, 1))
  end

  # If regular is true, retuns an array of the names of methods publicly
  # accessible in receiver and receiver's ancestors.  Otherwise, returns
  # an array of the names of receiver's singleton methods.
  def public_methods(regular = true)
    self.methods(regular)
  end

  def singleton_method_added(a_symbol)
   # invoked from code in .mcz when a singleton method is compiled
   # overrides the bootstrap implementation in Object_ruby.gs
   self
  end
  #  cannot make singleton_method_added private yet

  def singleton_method_removed(a_symbol)
   self
  end
  #  cannot make singleton_method_removed private yet

  def singleton_method_undefined(a_symbol)
   self
  end
  #  cannot make singleton_method_undefined private yet

  def tap(&block) # added for 1.8.7
    block.call(self)
    self
  end

  def __to_proc_arg
     # used in BlockPassNode IR
    self.to_proc
  end

  def to_a
     # remove this method for MRI 1.9 compatibility
     [ self ]
  end

  def to_enum(sym = :each , *args)  # added in 1.8.7
    # the receiver must implement the specified method that
    #  would return an enumerator
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added # fix infinite recursion if s.b. implements #each and
                 # calls #to_enum in there
      ts.remove(self)
      return self.enum_for(sym, *args)
    end
    return self.__send__(sym, *args)
  ensure
    ts.remove(self)
  end

  def to_fmt
    to_s
  end

  def to_s
    self.class.name.to_s
  end

  def __cext_to_s
    str = "#<"
    str << self.class.name
    str <<  ?:
    str << self.__id.to_s
    str << ?>
  end

  def __regex_to_s
    self.to_s
  end

  # Object should NOT have a to_str.  If to_str is implementd by passing
  # to to_s, then by default all objects can convert to a string!  But we
  # want classes to make an effort in order to convert their objects to
  # strings.
  #
  # def to_str
  #   to_s
  # end

  def __k_to_int
    # sent from C code in primitive 767 for sprintf
    v = nil
    begin
      v = self.to_int
    rescue
      begin
        v = Kernel.Integer(self)
      rescue
        # ignore
      end
    end
    v
  end

  ############################################################
  # MagLev API methods on Object
  #
  # These methods are part of MagLev's persistence and debug APIs.
  #
  # TODO:
  #   Should we wrap these in a module and only include them
  #   if requested?
  ############################################################

  # Swaps the identities of the receiver and the argument.
  #
  # Intended only for experienced MagLev programmers who need to migrate
  # instances of one class to another.
  #
  # The sender is responsible for checking the consistency of the class
  # histories of the argument and the receiver.  This method makes no
  # such checks (not applicable to MagLev).
  #
  # The argument, the receiver, or both are permitted to be invariant.
  #
  # Neither the argument nor the receiver may be special objects (instances of
  # classes such as SmallInteger, Character, or Boolean).  Also, neither may be
  # instances of a class that is a kind of
  #
  #    GsProcess, VariableContext, BlockClosure, GsSocket, GsFile, GsNMethod,
  #    CLibrary, CFunction, CPointer, CByteArray, Regexp, or GsCompilerNode.
  #
  # Neither the argument nor the receiver may be a kind of Bag that has
  # indexes built on it.  If either the receiver or the argument (or
  # both) participate in an index, then either both must be in byte
  # format or neither must be in byte format.  That is, one cannot be in
  # byte format if the other is not also.  To avoid the error conditions
  # triggered by presence of indexes, remove the indexes from the
  # relevant NSCs prior to invoking this method.
  #
  # Neither the argument nor the receiver may exist as self below the
  # sender of a become: message on the active MagLev stack.
  #
  # Once the identities have been swapped, the argument and receiver may
  # no longer satisfy the constraints of objects that reference them.
  # This condition can lead to the failure of subsequent index creation
  # attempts.  The MagLev programmer is responsible for correcting broken
  # constraints.
  #
  # Any clusterIds that belong to an object on disk remain with the
  # object.  That is, the clusterIds do not follow the identities when
  # they are swapped.
  #
  # The ObjectSecurityPolicies of the objects do not follow the
  # identities when they are swapped.
  #
  # As of Gs64 v3.0, tags are no longer swapped between the objects, they
  # are treated same as instance variables.
  #
  primitive_nobridge 'become', 'become:'

  # returns true if the receiver existed in GemStone at the time the
  # current transaction began.  Returns false otherwise.
  primitive_nobridge 'committed?', 'isCommitted'

  # Returns an Array of objects in the current session's temporary object
  # memory that reference the receiver.  The search continues until all
  # such objects have been found.  The result may contain both permenent
  # and temporary objects and may vary from run to run.  Does not abort
  # the current transaction.
  primitive_nobridge 'find_references_in_memory', 'findReferencesInMemory'

  # MaglevUndefined in Object1.rb
end
Object.__freeze_constants

