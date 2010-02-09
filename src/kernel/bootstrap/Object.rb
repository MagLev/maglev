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

    # End private helper methods

    primitive_nobridge '==', '='
    primitive 'halt'
    primitive 'hash'
    primitive 'object_id', 'asOop'
    primitive '__id__' , 'asOop'
    primitive '__identity_hash', 'identityHash'

    primitive 'nil?' , '_rubyNilQ'

    # compiler translates ! and  not   tokens to  _not 
    def not
      self._not # _not is a special send compiled direct to a bytecode
    end

    # rubySend: methods implemented in .mcz
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

    #  'send:*&' , '__send__:*&' special cased in  installBridgeMethodsFor ,
    #   to have no bridges.
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
    #   for the number of with: keywords;
    #   and last arg is envId
    # __perform are used by RubyParser and FFI::StructLayout 
    primitive_nobridge '__perform_se', 'with:perform:env:' 
    primitive_nobridge '__perform__se', 'with:with:perform:env:' 
     
    # redefinition of __perform_method disallowed after bootstrap,
    #  it is used by implementation of eval
    primitive_nobridge '__perform_meth', 'performMethod:'

    primitive   '__basic_dup', '_rubyBasicDup'      # use non-singleton class
    primitive   '__basic_clone', '_basicCopy' # use singleton class

    def dup
      res = self.__basic_dup
      res.initialize_copy(self)
      res
    end

    def clone
      res = self.__basic_clone
      res.initialize_copy(self)
      if self.frozen?
        res.freeze
      end
      res
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

    def respond_to?(symbol, include_private)
      __responds_to(symbol, include_private, 0x10101)
    end

    def respond_to?(symbol )
      __responds_to(symbol, false, 0x10101)
    end

    def __splat_lasgn_value
      # runtime support for   x = *y   , invoked from generated code
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

    def __splat_lasgn_value_coerce
      v = self
      begin
        v = self.to_ary
      rescue
        # ignore if not responding to to_ary
      end
      if v._not_equal?(self)
        unless v._isArray
          raise TypeError, 'arg to splat responded to to_ary but did not return an Array'
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
      a
    end

    def __splat_return_value
      # runtime support for  return *v  , invoked from generated code
      v = Type.coerce_to_or_nil(self, Array, :to_ary)
      if v._equal?(nil)
        v = Type.coerce_to_or_nil(self, Array, :to_a)
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
      elsif self.respond_to?(:to_ary)
        return self.to_ary
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

    #  following 3 prims must also be installed in Behavior
    primitive_nobridge '__instvar_get', 'rubyInstvarAt:'
    primitive_nobridge '__instvar_put', 'rubyInstvarAt:put:'
    primitive_nobridge 'instance_variables', 'rubyInstvarNames'

    def instance_variable_get(a_name)
      unless a_name._isStringOrSymbol
        a_name = Type.coerce_to(a_name, String, :to_str)
      end
      __instvar_get(a_name.to_sym)
    end

    def instance_variable_set(a_name, a_val)
      unless a_name._isStringOrSymbol
        a_name = Type.coerce_to(a_name, String, :to_str)
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
      self == other
    end

    def extend(*modules)
      if (modules.length > 0)
        cl = class << self
               self
             end
        modules.each do |aModule|
          cl.include(aModule)
          aModule.extended(self)
        end
      end
      self
    end

    def initialize(*args)
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

    def initialize_copy(other)
      # dup and clone are complete, and C extensions not supported yet,
      #   so do nothing
      self
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

    primitive_nobridge '__instance_eval', 'instanceEvalString:with:binding:'

    def instance_eval(__lex_path, str, file, *args)
      # __lex_path arg is synthesized by the parser in calling code
      # TODO: Object#instance_eval: handle file and line params
      if args.size > 1
        raise ArgumentError, 'too many args'
      end
      string = Type.coerce_to(str, String, :to_str)
      ctx = self.__binding_ctx(0)
      bnd = Binding.new(ctx, self, nil)
      bnd.__set_lex_scope(__lex_path)
      vcgl = [ self.__getRubyVcGlobal(0x20),
               self.__getRubyVcGlobal(0x21) ]
      blk = bnd.block
      unless blk._equal?(nil)
        vcgl << blk
      end
      res = __instance_eval(string, vcgl, bnd )
      vcgl[0].__storeRubyVcGlobal(0x20)
      vcgl[1].__storeRubyVcGlobal(0x21)
      res
    end

    def instance_eval(__lex_path, str, file)
      # __lex_path arg is synthesized by the parser in calling code
      # TODO: Object#instance_eval: handle file and line params
      string = Type.coerce_to(str, String, :to_str)
      ctx = self.__binding_ctx(0)
      bnd = Binding.new(ctx, self, nil)
      bnd.__set_lex_scope(__lex_path)
      vcgl = [ self.__getRubyVcGlobal(0x20),
               self.__getRubyVcGlobal(0x21) ]
      blk = bnd.block
      unless blk._equal?(nil)
        vcgl << blk
      end
      res = __instance_eval(string, vcgl, bnd )
      vcgl[0].__storeRubyVcGlobal(0x20)
      vcgl[1].__storeRubyVcGlobal(0x21)
      res
    end

    def instance_eval(__lex_path, str)
      # __lex_path arg is synthesized by the parser in calling code
      string = Type.coerce_to(str, String, :to_str)
      ctx = self.__binding_ctx(0)
      bnd = Binding.new(ctx, self, nil)
      bnd.__set_lex_scope(__lex_path)
      vcgl = [ self.__getRubyVcGlobal(0x20),
               self.__getRubyVcGlobal(0x21) ]
      blk = bnd.block
      unless blk._equal?(nil)
        vcgl << blk
      end
      res = __instance_eval(string, vcgl, bnd )
      vcgl[0].__storeRubyVcGlobal(0x20)
      vcgl[1].__storeRubyVcGlobal(0x21)
      res
    end

    def instance_eval(str) 
      # could be sent via  __send__ or send, but not supported yet
      # You must code evals explicitly.
      raise ArgumentError, 'too few args, send of :instance_eval not supported'
    end

    primitive_nobridge_env 'instance_eval&', 'rubyEval', ':block:'
      # __lex_path arg from the parser  is ignored , because there is no
      #   source string to compile.
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

    # Object should NOT have a to_str.  If to_str is implementd by passing
    # to to_s, then by default all objects can convert to a string!  But we
    # want classes to make an effort in order to convert their objects to
    # strings.
    #
    # def to_str
    #   to_s
    # end

    primitive_nobridge '__ruby_singleton_methods', 'rubySingletonMethods:protection:'

    def singleton_methods(inc_modules = true)
      __ruby_singleton_methods(inc_modules, 0)
    end

    primitive_nobridge '__ruby_methods', 'rubyMethods:'

    # If regular is true, retuns an array of the names of methods publicly
    # accessible in receiver and receiver's ancestors.  Otherwise, returns
    # an array of the names of receiver's singleton methods.
    def methods(regular = true)
      if regular
        __ruby_methods(0) # get public methods
      else
        __ruby_singleton_methods(false, 0)
      end
    end

    def private_methods
      __ruby_methods(2)
    end

    def protected_methods
      __ruby_methods(1)
    end

    def public_methods
      __ruby_methods(0)
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

    # TODO   singleton_method_removed

    def to_a
       # remove this method for MRI 1.9 compatibility
       [ self ]
    end

    def to_fmt
      to_s
    end

    def to_s
      self.class.name.to_s
    end

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

    # returns true if the receiver existed in GemStone at the time the
    # current transaction began.  Returns false otherwise.
    primitive_nobridge 'committed?', 'isCommitted'

    # Returns an Array of objects in the current session's temporary object
    # memory that reference the receiver.  The search continues until all
    # such objects have been found.  The result may contain both permenent
    # and temporary objects and may vary from run to run.  Does not abort
    # the current transaction.
    primitive_nobridge 'find_references_in_memory', 'findReferencesInMemory'
end


# Undefined is a sentinal value used to distinguish between nil as a value passed 
# by the user and the user not passing anything for a defaulted value.  E.g.,:
#
#   def foo(required_param, optional_param=Undefined)
#     if optional_param._equal?( Undefined )
#       puts "User did not pass a value"
#     else
#       puts "Users passed #{optional_param} (which may be nil)"
#     fi
#   end
#
Undefined = Object.new

