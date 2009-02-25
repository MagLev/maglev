class Object
  include Kernel

    # Begin private helper methods

    # primitive_nobridge has max of 3 normal args, 1 star arg, 1 block arg
    #  example with a block argument
    #    primitive_nobridge '_foo*&' , '_foo:a:b:c:d:e:'
    #  example without a block argument
    #    primitive_nobridge '_foo*' , '_foo:a:b:c:d:'


    #  Begin special sends
    #    Reimplementation of these selectors is disallowed by the parser
    #    and they are optimized by code generator to special bytecodes.
    #    Attempts to reimplement them will cause compile-time errors.
    #    Entries here are so that perform will work.
    # _isInteger allows integer?  special sends to non-Numeric objects
    primitive_nobridge '_isArray', '_isArray'
    primitive_nobridge '_isBlock', '_isExecBlock'
    primitive_nobridge '_isFixnum', '_isSmallInteger'
    primitive_nobridge '_isFloat', '_isFloat'
    primitive_nobridge '_isHash', '_isRubyHash'
    primitive_nobridge '_isInteger', '_isInteger'
    primitive_nobridge '_isNumber', '_isNumber'
    primitive_nobridge '_isRegexp', '_isRegexp'
    primitive_nobridge '_isRange', '_isRange'
    primitive_nobridge '_isString', '_isRubyString'  # returns false for Symbols
    primitive_nobridge '_isSymbol', '_isSymbol'      # returns false for Strings
    primitive_nobridge '_isStringOrSymbol', '_isOneByteString'
    # End special sends

    #  following are installed by RubyContext>>installPrimitiveBootstrap
    #    primitive_nobridge 'class', 'class' # installed in Object
    #  end installPrimitiveBootstrap

    #  Private method _each: contains on:do: handler for RubyBreakException ,
    #  all env1 sends of each& are compiled as sends of _each&
    #  Attempts to reimplement _each& will fail with a compile error.
    primitive_nobridge '_each&', '_rubyEach:'

    # _storeRubyVcGlobal is used by methods that need to store into
    #   caller(s) definition(if any) of $~ or $_  .
    #  Receiver is value to be stored.
    #  See smalltalk code in Object for documentation.
    primitive_nobridge '_storeRubyVcGlobal' , '_storeRubyVcGlobal:'
    #  _getRubyVcGlobal returns caller(s) value of $~ or $_ , or nil
    primitive_nobridge '_getRubyVcGlobal' , '_getRubyVcGlobal:'

    # End private helper methods

    primitive_nobridge '==', '='
    primitive 'halt'
    primitive 'hash'
    primitive 'object_id', 'asOop'
    primitive '__id__' , 'asOop'

    #  not   is a special selector,
    #   GsComSelectorLeaf>>selector:  translates #not to bytecode Bc_rubyNOT
    # primitive 'not'

    primitive 'nil?' , '_rubyNilQ'

    # rubySend: methods implemented in .mcz
    primitive_nobridge 'send',  'rubySend:'
    primitive_nobridge 'send',  'rubySend:with:'
    primitive_nobridge 'send',  'rubySend:with:with:'
    primitive_nobridge 'send',  'rubySend:with:with:with:'
    primitive_nobridge 'send&', 'rubySend:block:'
    primitive_nobridge 'send&', 'rubySend:with:block:'
    primitive_nobridge 'send&', 'rubySend:with:with:block:'
    primitive_nobridge 'send&', 'rubySend:with:with:with:block:'
    primitive          'send*&' , 'rubySend:withArgs:block:'

    #  'send:*&' , '__send__:*&' special cased in  installBridgeMethodsFor ,
    #    any other   def send;...  end   gets no bridges  during bootstrap
    #    to allow reimplementation of  send  for methods updating $~ , $_

    #  __send__ defined per MRI, non-overrideable version of send
    #  redefinition of __send__  disallowed by parser after bootstrap finished.
    primitive_nobridge '__send__',  'rubySend:'
    primitive_nobridge '__send__',  'rubySend:with:'
    primitive_nobridge '__send__',  'rubySend:with:with:'
    primitive_nobridge '__send__',  'rubySend:with:with:with:'
    primitive_nobridge '__send__&', 'rubySend:block:'
    primitive_nobridge '__send__&', 'rubySend:with:block:'
    primitive_nobridge '__send__&', 'rubySend:with:with:block:'
    primitive_nobridge '__send__&', 'rubySend:with:with:with:block:'
    primitive          '__send__*&' , 'rubySend:withArgs:block:'

    primitive 'dup', '_basicCopy'
    primitive 'clone', '_basicCopy'

    primitive 'freeze', 'immediateInvariant'
    primitive 'frozen?', 'isInvariant'

    # install this prim so  anObj.send(:kind_of?, aCls)   will work
    primitive_nobridge 'kind_of?' , '_rubyKindOf:'

    # install this prim so  anObj.send(:is_a?, aCls)   will work
    primitive_nobridge 'is_a?' , '_rubyKindOf:'

    primitive_nobridge '_responds_to', '_respondsTo:private:flags:'
       # _responds_to flags bit masks are
       #     environmentId                   0xFF
       #     ruby lookup semantics          0x100
       #     ruby receiver is self         0x1000 (for future use)
       #     cache successes in code_gen  0x10000
    
    def respond_to?(symbol, include_private)
      _responds_to(symbol, include_private, 0x10101)
    end

    def respond_to?(symbol )
      _responds_to(symbol, false, 0x10101)
    end

    def _splat_return_value
      # runtime support for  return *v  , invoked from generated IR
      v = self
      unless v._isArray
        begin
          v = Type.coerce_to(self, Array, :to_ary)
        rescue TypeError
          begin 
            v = Type.coerce_to(self, Array, :to_a)
          rescue TypeError
            # ignore
          end 
        end
      end
      sz = v.length
      if sz < 2
	if sz.equal?(0)
	  return nil
	else
	  return v[0]
	end
      else
	return v
      end
    end

    primitive 'print_line', 'rubyPrint:'

    # pause is not standard Ruby, for debugging only .
    #  trappable only by an Exception specifying exactly error 6001
    primitive 'pause', 'pause'

    #                                   rubyInspect comes from .mcz
    primitive_nobridge '_inspect', '_rubyInspect:'

    def inspect(touchedSet=nil)
      self._inspect(touchedSet)
    end

    #  following 3 prims must also be installed in Behavior
    primitive_nobridge '_instVarAt', 'rubyInstvarAt:'
    primitive_nobridge '_instVarAtPut', 'rubyInstvarAt:put:'
    primitive_nobridge 'instance_variables', 'rubyInstvarNames'

    def instance_variable_get(a_name)
      unless a_name._isStringOrSymbol
        a_name = Type.coerce_to(a_name, String, :to_str)
      end
      unless (a_name[0].equal?( ?@ ))
        raise NameError, "`#{a_name}' is not allowed as an instance variable name"
      end
      _instVarAt(a_name.to_sym)
    end

    def instance_variable_set(a_name, a_val)
      unless a_name._isStringOrSymbol
        a_name = Type.coerce_to(a_name, String, :to_str)
      end
      unless (a_name[0].equal?( ?@ ))
        raise NameError, "`#{a_name}' is not allowed as an instance variable name"
      end
      _instVarAtPut(a_name.to_sym, a_val)
      a_val
    end

    primitive_nobridge 'method', 'rubyMethod:'

    def ===(obj)
        self == obj
    end

    # block_given?  is implemented by the ruby parser .
    #   do not code any definition of block_given? here .
    # Attempts to reimplement  block_given?  will fail with a compile error.

    def initialize(*args)
       self
    end

    def initialize_copy(other)
      # dup and clone are complete, and C extensions not supported yet,
      #   so do nothing
      self
    end

    # equal?  is implemented by the ruby parser and optimized to
    #  a special bytecode by the code generator.
    # Attempts to reimplement equal? will fail with a compile error.

    def eql?(other)
      self == other
    end

  def extend(*modules)
    if (modules.length > 0)
      cl = class << self
        self
      end
      modules.each{ |aModule| cl.include(aModule) }
    end
    self
  end

  def flatten_onto(output)
    output << self
    output
  end

    def pretty_inspect
      # TODO: remove
      # hides Kernel#pretty_inspect from mspec's src/lib/pp.rb 
      #   until we get Thread implemented.
      inspect;
    end


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

    primitive_nobridge '_instance_eval', 'rubyEvalString:with:'

    def instance_eval(*args)
      # bridge methods would interfere with VcGlobals logic
      raise ArgumentError, 'wrong number of args'
    end

    def instance_eval(str)
      string = Type.coerce_to(str, String, :to_str)
      vcgl = [ self._getRubyVcGlobal(0x20),
               self._getRubyVcGlobal(0x21) ]
      res = _instance_eval(string, vcgl)
      vcgl[0]._storeRubyVcGlobal(0x20)
      vcgl[1]._storeRubyVcGlobal(0x21)
      res
    end

    def instance_eval(str, file=nil)
      string = Type.coerce_to(str, String, :to_str)
      vcgl = [ self._getRubyVcGlobal(0x20),
               self._getRubyVcGlobal(0x21) ]
      res = _instance_eval(string, vcgl)
      vcgl[0]._storeRubyVcGlobal(0x20)
      vcgl[1]._storeRubyVcGlobal(0x21)
      res
    end

    def instance_eval(str, file=nil, line=nil)
      # TODO: Object#instance_eval: handle file and line params
      string = Type.coerce_to(str, String, :to_str)
      vcgl = [ self._getRubyVcGlobal(0x20),
               self._getRubyVcGlobal(0x21) ]
      res = _instance_eval(string, vcgl)
      vcgl[0]._storeRubyVcGlobal(0x20)
      vcgl[1]._storeRubyVcGlobal(0x21)
      res
    end

    primitive_nobridge 'instance_eval&', 'rubyEval:'

    # Object should NOT have a to_str.  If to_str is implementd by passing
    # to to_s, then by default all objects can convert to a string!  But we
    # want classes to make an effort in order to convert their objects to
    # strings.
    #
    # def to_str
    #   to_s
    # end

    primitive_nobridge '_ruby_singleton_methods', 'rubySingletonMethods:protection:'

    def singleton_methods(inc_modules = true)
      _ruby_singleton_methods(inc_modules, 0)
    end

    primitive_nobridge '_ruby_methods', 'rubyMethods:'

    # If regular is true, retuns an array of the names of methods publicly
    # accessible in receiver and receiver's ancestors.  Otherwise, returns
    # an array of the names of receiver's singleton methods.
    def methods(regular = true)
      if regular
        _ruby_methods(0) # get public methods
      else
        _ruby_singleton_methods(false, 0)
      end
    end

    def private_methods(unused_boolean=true)
      _ruby_methods(2) 
    end

    def protected_methods(unused_boolean=true)
      _ruby_methods(1) 
    end

    def _isBehavior
      false
    end

    def instance_of?(cls)
      # Modified from RUBINIUS
      if self.class.equal?(cls)
        true
      else
        unless cls._isBehavior
          raise TypeError, 'expected a Class or Module'
        end
        false
      end
    end
end


# Sentinal value used to distinguish between nil as a value passed by the
# user and the user not passing anything for a defaulted value.  E.g.,:
#
#   def foo(required_param, optional_param=Undefined)
#     if optional_param.equal?( Undefined )
#       puts "User did not pass a value"
#     else
#       puts "Users passed #{optional_param} (which may be nil)"
#     fi
#   end
#
Undefined = Object.new

