module FFI

  # Maglev does not yet support:
  #    FFI::Union
  #    FFI::Library.callback

  # Smalltalk implementation classes
  class CLibrary
    class_primitive_nobridge 'named' , 'named:'
    class_primitive_nobridge '__has_symbol', 'hasCSymbol:'
    primitive_nobridge '__has_symbol', 'hasCSymbol:'
    primitive_nobridge 'name', 'name'

    def self.__set_libraries(lib_names , clibraries )
      if RubyContext.persistence_mode
        arr = PersistentLibraries
        arr[0] = lib_names
        arr[1] = clibraries
      end
      $__FFI_CLibrary_TransientLibraries = [ lib_names, clibraries ]
    end

    def self.__libraries
      # returns an Array,   [ lib_names, clibraries ]
      if defined?($__FFI_CLibrary_TransientLibraries)
        return $__FFI_CLibrary_TransientLibraries
      end
      PersistentLibraries
    end

    def self.__library_names
      self.__libraries[0]
    end

    def self.__clibraries
      self.__libraries[1]
    end

    # Register the zero argument block to be called each time the
    # shared library represented by the receiver is loaded into a VM  .
    # The block is also run when initialize_on_load is called,
    # if there was no previous block registered with the receiver.
    # For an example, see lib/ruby/1.8/maglev/opensslffi/libcrypto.rb .
    def initialize_on_load(&block)
      old = @_st_onLoadBlock
      @_st_onLoadBlock = block
      if old._equal?(nil) && block_given?
        block.call
      end
    end
  end

  class CCallout
    primitive_nobridge 'call_template*' , '_rubyToCcallTemplate:'

    class_primitive_nobridge '__new', '_rubyNew:'
     # arg is [ cLibrary, fName, resType, argTypesArray, varArgsAfter]

    primitive_nobridge '__compile_caller', '_compileCaller:In:enums:'
  end

  class CCallin

    class_primitive_nobridge '__new', '_rubyNew:'
    # arg is [ fName, resType, argTypesArray ]

  end

  class Pointer # [

    primitive_nobridge '[]', '_rubyByteAt:'
    primitive_nobridge 'size', 'size'

    # following used for fields of Struct
    primitive_nobridge 'int8at', 'int8At:'
    primitive_nobridge 'get_char', 'int8At:'
    primitive_nobridge 'get_uchar', 'uint8At:'
    primitive_nobridge 'get_short', 'int16At:'
    primitive_nobridge 'get_ushort', 'uint16At:'
    primitive_nobridge 'get_int', 'int32At:'
    primitive_nobridge 'get_uint', 'uint32At:'
    primitive_nobridge 'int64at', 'int64At:'
    primitive_nobridge 'uint64at', 'uint64At:'
    primitive_nobridge 'get_long', 'int64At:'
    primitive_nobridge 'get_long_long', 'int64At:'
    primitive_nobridge 'get_int64', 'int64At:'
    primitive_nobridge 'get_ulong', 'uint64At:'
    primitive_nobridge 'get_ulong_long', 'uint64At:'
    primitive_nobridge 'double_at', 'doubleAt:'
    primitive_nobridge 'float_at', 'floatAt:'
    primitive_nobridge 'get_float32', 'floatAt:'
    primitive_nobridge 'get_float', 'floatAt:'
    primitive_nobridge 'get_float64', 'doubleAt:'
    primitive_nobridge 'get_double', 'doubleAt:'
    primitive_nobridge 'float_put',  'floatAt:truncatePut:'
    primitive_nobridge 'double_put', 'doubleAt:truncatePut:'
    primitive_nobridge 'put_char', 'int8At:truncatePut:'
    primitive_nobridge 'int8_put', 'int8At:truncatePut:'
    primitive_nobridge 'put_uchar', 'int8At:truncatePut:'
    primitive_nobridge 'put_short', 'int16At:truncatePut:'
    primitive_nobridge 'put_ushort', 'int16At:truncatePut:'
    primitive_nobridge 'put_int', 'int32At:truncatePut:'
    primitive_nobridge 'put_int32', 'int32At:truncatePut:'
    primitive_nobridge 'put_uint', 'int32At:truncatePut:'
    primitive_nobridge 'put_uint32', 'int32At:truncatePut:'
    primitive_nobridge 'int64_put', 'int64At:truncatePut:'
    primitive_nobridge 'put_long' , 'int64At:truncatePut:'
    primitive_nobridge 'put_ulong' , 'int64At:truncatePut:'
    primitive_nobridge 'put_long_long' , 'int64At:truncatePut:'
    primitive_nobridge 'put_ulong_long' , 'int64At:truncatePut:'
    primitive_nobridge '__unsigned_wordsize_at', '_unsigned:at:'
    primitive_nobridge '__signed_wordsize_at', '_signed:at:with:'
    primitive_nobridge 'total', 'size'

    # more variants used by Pointer, Buffer , Struct

    primitive_nobridge 'stringfrom_to', 'stringFrom:to:'
      # zero-based start offset,  zero-based end offset(NOT limit)

    primitive_nobridge 'char_star_at' , 'stringFromCharStarAt:'
    primitive_nobridge 'char_star_put' , 'pointerAt:put:'
    primitive_nobridge '__pointer_at' , 'pointerAt:resultClass:'

    # def __pointer_at_put(byteoffset, pointer) ; end
    #   pointer is a kind of Pointer or a CPointer, or nil
    primitive_nobridge '__pointer_at_put' , 'pointerAt:put:'

    primitive_nobridge '__set_derived_from' , 'derivedFrom:'

    primitive_nobridge 'copyfrom_from_to_into', 'copyFrom:from:to:into:'
       # object, one-based start, one-based end, zero-based dest offset

    class_primitive_nobridge '__gc_malloc' , 'gcMalloc:'
      # allocates C memory which is auto-freed when instance is GC'ed

    class_primitive_nobridge '__malloc' , 'malloc:'
      # C memory is not auto-freed , arg -1 means encapsulate NULL

    class_primitive_nobridge '__from_address', 'fromAddress:'

    primitive_nobridge 'address' , 'memoryAddress'

    primitive_nobridge 'autorelease', 'autoRelease'

    primitive_nobridge 'autorelease=', 'autoRelease:'
      # argument must be a boolean, false means disable auto-free
      #  all other values ignored.

    primitive_nobridge 'free' , 'setDead'
      # does not actually free C memory. subsequent attempts
      # to access C memory will raise an exception.

    primitive_nobridge 'memset' , 'memset:from:to:'
     # args are  ushort value, zero-based start offset,
     #    zero-based end offset (-1 means to end of allocated C memory)

    # methods used to create derived instances of regions of Structs, etc
    class_primitive_nobridge '__fromRegionOf' , 'fromRegionOf:offset:numBytes:'
    class_primitive_nobridge '__fromCPointer' , 'fromCPointer:numBytes:'

    def __struct_pointer_at(byteoffset)
      return self.__pointer_at(byteoffset, MemoryPointer)
    end

    def __search_for_zerobyte(offset)
      # result -1 if no null byte found
      self.__unsigned_wordsize_at(-1, offset)
    end


    def clear
      self.memset(0, 0, -1)
    end

    primitive_nobridge '__inspect' , '_inspect'

    def self.__new_null
      __malloc(-1)
    end

    # remainder of Pointer methods in pointer.rb

  end # ]


  # A CPointer encapsulates a C pointer  which does not have auto-free
  # semantics.  New instances are created by CFunction calls with result
  # type #ptr, and are also used for certain arguments of CFunctions.
  #
  # A CPointer may be "derived from" some other pointer (e.g., it is a
  # pointer to a field in a struct, which itself is represented by a
  # pointer).  Derived pointers maintain a reference to the base pointer to
  # pin the memory from GC.
  class CPointer # [
    primitive_nobridge 'address' , 'memoryAddress'

    class_primitive_nobridge '__new_from', 'newFrom:'
    class_primitive_nobridge '__new_null', 'newNull'

    def ==(pointer)
      unless pointer._kind_of?(Pointer) || pointer._kind_of?(CPointer)
        return false
      end
      pointer.address == self.address
    end

    def null?
      self.address._equal?(0)
    end

    def hash
      self.address.hash
    end
  end # ]

  class Type
    def self.__PersistentTypes
      PersistentTypes  # defined in ffi.rb
    end
    def self.__TransientTypes
      if defined?($__FFI_Type_TransientTypes)
        ht = $__FFI_Type_TransientTypes
      else
        ht = IdentityHash.new
        $__FFI_Type_TransientTypes = ht
      end
      ht
    end
    def self.__find_type(name)
      unless name._isSymbol
        raise TypeError , 'ruby_name must be a Symbol'
      end
      tr_types = self.__TransientTypes
      t = tr_types[name]
      if t._not_equal?(nil)
         return t
      end
      p_types = PersistentTypes
      t = p_types[name]
      if t._not_equal?(nil)
         return t
      end
      PrimTypeDefs[name]
    end
    def self.__add_type(name, val)
      tr_types = self.__TransientTypes
      tr_types[name] = val
      if RubyContext.persistence_mode
        PersistentTypes[name] = val
      end
    end

    def self.__install_native_type(a_type)
      FFI.const_set( a_type.name , a_type )
    end
    def self.__initialize_native_types
      # install various module level constants which are names for primitive types
      __install_native_type( Type.new( :TYPE_INT8, 1 , :int8))  # NativeType::INT8 in MRI implem
      __install_native_type( Type.new( :TYPE_UINT8, 1 , :uint8))
      __install_native_type( Type.new( :TYPE_INT16, 2 , :int16))
      __install_native_type( Type.new( :TYPE_UINT16, 2 , :uint16))
      __install_native_type( Type.new( :TYPE_INT32, 4 , :int32))
      __install_native_type( Type.new( :TYPE_UINT32, 4 , :uint32))
      __install_native_type( Type.new( :TYPE_INT64, 8 , :int64))
      __install_native_type( Type.new( :TYPE_UINT64, 8 , :uint64))
      __install_native_type( Type.new( :TYPE_FLOAT32, 4 , :float))
      __install_native_type( Type.new( :TYPE_FLOAT64, 8 , :double))
      __install_native_type( Type.new( :TYPE_VOID, 8 , :void))
      __install_native_type( Type.new( :TYPE_STRING, 8 , :string))
      __install_native_type( Type.new( :TYPE_BUFFER_INOUT, 8 , :ptr))
      __install_native_type( Type.new( :TYPE_BUFFER_IN, 8 , :ptr))
      __install_native_type( Type.new( :TYPE_BUFFER_OUT, 8 , :ptr))
      # ENUM, BOOL, CHAR_ARRAY, VARARGS  todo
    end

    # instance methods
    def name
      @name
    end
    def size
      @size
    end
    def alignment
      @size  # all types have alignment equal to their size
    end
    def __prim_type
      @prim_type_name
    end
  end
  Type.__initialize_native_types

  module_function() # [ ===================== following are module functions
    # these are after  'module_function' instead of inside of a 'class << self'
    # to avoid dynamic constant refs .

    def errno
      Dir.__get_clear_errno
    end

    def find_type(query)
      if query._isSymbol
        t = Type.__find_type(query)
        if t._not_equal?(nil)
           return t
        end
      end
      if query._kind_of?(Struct.class)
        return :ptr
      end
      if query._kind_of?(Enum)
        t = Enums.find(query)
        if t._not_equal?(nil)
           return t
        end
      end
      raise TypeError, "Unable to resolve FFI type '#{query}'"
    end

    def find_base_type(query)
      prev = query
      set = IdentitySet.new
      while true
        unless set.__add_if_absent(prev)
          raise RuntimeError, 'infinite loop in find_base_type'
        end
        t = find_type(prev)
        if t._isSymbol
          return t
        elsif t._kind_of?(Enum)
          return t
        elsif t._kind_of?(CCallin)
          return t
        elsif t._kind_of?(Type)
          prev = t
        else
          raise TypeError, 'result of find_type is not a Type, Enum , callback, or Symbol'
        end
      end
    end

    def type_size(type)
      t = find_base_type(type)
      size = PrimTypeSizes[t]
      if size._equal?(nil)
        unless type._isSymbol
          raise TypeError, "FFI::type_size - type argument must be a Symbol"
        else
          raise TypeError, "FFI::type_size to resolve FFI type '#{type}'"
        end
      end
      size
    end

    def size_to_type(size)
      if size._equal?(4)
        return :int
      elsif size._equal?(2)
        return :short
      elsif size._equal?(1)
        return :char
      elsif size._equal?(8)
        return :long
      else
        raise ArgumentError , 'unsupported size'
      end
    end
  # end module_function() ]

  module Library # [
    # Set which library or libraries +attach_function+ should
    # look in. By default it only searches for the function in
    # the current process. If you want to specify this as one
    # of the locations, add FFI::Library::CURRENT_PROCESS
    # The libraries are tried in the order given.
    #
    def ffi_lib(*names)
      len = names.length
      libs = []
      lib_names = []
      n = 0
      my_debug = FFI::DEBUG
      while n < len
        a_name = names[n]
        if  my_debug > 0
          debug_name = a_name == CURRENT_PROCESS ?
            "CURRENT_PROCESS" : a_name.inspect
          puts "--FFI:  ffi_lib: adding #{debug_name}"
        end
        if a_name == CURRENT_PROCESS || a_name._equal?(nil)
          libs << nil
        else
          a_name = Maglev::Type.coerce_to(names[n], String, :to_str)
          libs << CLibrary.named(a_name)
        end
        lib_names << a_name
        n += 1
      end
      CLibrary.__set_libraries( lib_names, libs ).last
    end

    def ffi_libraries
      CLibrary.__library_names
    end

    # define a callback type
    #
    def callback(name_sym, arg_types, return_type=MaglevUndefined)
      if return_type._equal?(MaglevUndefined)
        # per JRuby, assume 2 args and name_sym was omitted
        raise ArgumentError, 'anonymous callbacks not supported'
      end
      unless name_sym._isSymbol
        raise TypeError, 'name of a callback must be a Symbol'
      end
      prev_cb = Type.__find_type(name_sym)
      if prev_cb._not_equal?(nil)
        raise ArgumentError, "a callback was already defined with name #{name_sym}"
      end
      ffimod = FFI
      st_argnum = 1
      base_arg_types = []
      arg_types.each { |t|
        if t._equal?(:varargs)
          raise ArgumentError, ":varargs not supported for callbacks"
        else
          bt = ffimod.find_base_type(t)
          if bt._isSymbol
            base_arg_types << bt
          elsif bt._kind_of?(Enum)
            raise TypeError, "Enum not supported as type of an argument to a callback"
          elsif bt._kind_of?(CCallin)
            raise TypeError, "a callback is not supported as an argument to a callback"
          else
            raise TypeError, 'unrecognized base argument type #{bt}'
          end
          st_argnum += 1
        end
      }
      ret = ffimod.find_base_type(return_type)
      enum_ret = nil
      unless ret._isSymbol
        if ret._kind_of?(Enum)
          raise TypeError, "use of an Enum as a return type is not supported"
        elsif ret._kind_of?(CCallin)
          raise TypeError, "use of a callback as a return type is not supported"
        else
          raise TypeError, 'unrecognized base return type #{ret}'
        end
      end
      cb = CCallin.__new([ name_sym, ret, base_arg_types ])
      Type.__add_type( name_sym, cb )
      return cb
    end

    # Attach a C function to this module. The arguments can have two forms:
    #
    #   attach_function c_name, [c_arg1, c_arg2], ret
    #   attach_function mod_name, c_name, [c_arg1, c_arg2], ret
    #
    # In the first form, +c_name+ will also be used for the name of the module
    # method. In the second form, the module method name is +mod_name+.
    #
    # The +c_name+ and +mod_name+ can be given as Strings or Symbols.
    #
    # The types of the arguments to the C function, +c_arg1+, +c_arg2+, etc, are
    # given as an array even if there is only one.
    #
    # The final argument, +ret+, is the type of the return value from the C
    # function.
    # Returns a method or raises an error.
    def attach_function(name, a3, a4, a5=MaglevUndefined)
      name = Maglev::Type.coerce_to(name, String, :to_s)
      if a5._not_equal?(MaglevUndefined)
        cname = Maglev::Type.coerce_to(a3, String, :to_s)
        args = a4
        ret = a5
      else
        cname = name
        args = a3
        ret = a4
      end

      ffimod = FFI
      st_argnum = 1
      enum_args = []
      base_arg_types = []
      have_varargs = false
      var_args_after = -1
      args.each { |t|
        if t._equal?(:varargs)
          have_varargs = true
          var_args_after = base_arg_types.size
        elsif have_varargs
          raise TypeError , 'no more args allowed after :varargs'
        else
          bt = ffimod.find_base_type(t)
          if bt._isSymbol
            base_arg_types << bt
          elsif bt._kind_of?(Enum)
            enum_args << st_argnum ; enum_args << bt
            base_arg_types << :int64
          elsif bt._kind_of?(CCallin)
            base_arg_types << bt
          else
            raise TypeError, 'unrecognized base argument type #{bt}'
          end
          st_argnum += 1
        end
      }
      if enum_args.size._equal?(0)
        enum_args = nil
      end
      ret = ffimod.find_base_type(ret)
      enum_ret = nil
      unless ret._isSymbol
        if ret._kind_of?(Enum)
          enum_ret = ret
          ret = :int64
        elsif ret._kind_of?(CCallin)
          raise TypeError, "use of a callback as a return type is not supported"
        else
          raise TypeError, 'unrecognized base return type #{ret}'
        end
      end
      libs = CLibrary.__clibraries
      if libs._equal?(nil)
        libs = [ nil ]
      end
      my_debug = DEBUG
      n = 0
      len = libs.length
      if len._equal?(0)
        libs = [ nil ] # search process by default
        len = 1
      end
      while n < len
        lib = libs[n]
        if lib._equal?(nil)
          puts "--FFI:  attach_function: #{cname} searching process"   if my_debug > 1
          found = CLibrary.__has_symbol(cname) # check entire process
          if found && my_debug > 0
             puts "--FFI:  attach_function: found #{cname} in process"
          end
        else
          puts "--FFI:  attach_function: #{cname} searching lib #{lib.name}"   if my_debug > 1
          found = lib.__has_symbol(cname)
          if found && my_debug > 1
             puts "--FFI:  attach_function: found #{cname} in lib #{lib.name}"
          end
        end
        if found
          untagged_enums = Enums::__untagged_enums_asGsMethDict
          cf = CCallout.__new([ lib, cname, ret, base_arg_types, var_args_after, untagged_enums ])

          # install a method in self, derived from a rubyToCcallTemplate variant,
          #    which will be installed per RubyContext.persistent_mode
          meth = cf.__compile_caller(name, self, [ Enums , enum_args , enum_ret ] )
          return meth
        end
        n += 1
      end
      raise FFI::NotFoundError, "Unable to find FFI '#{cname}' in library: #{libs.inspect}"
    end

    def typedef(atype, new_name)
      unless new_name._isSymbol
        raise TypeError , 'name must be a Symbol'
      end
      if new_name._equal?(atype)
        raise TypeError , 'cannot define a type in terms of itself'
      end
      code = if atype._kind_of?(Type)
               atype
             elsif atype._equal?( :enum )
               if new_name._isArray
                 self.enum(new_name)
               else
                 # self.enum(info, new_name)  # don't understand what info is
                 raise TypeError , 'unrecognized args to typedef'
               end
             else
               FFI.find_type(atype)
             end
      Type.__add_type(new_name, code)
    end

    # Examples
    #   following define un-named enums
    # enum(:zero, :one, :two  ) # like C   enum { zero, one, two } foo ;
    # enum([ :zero, :one, :two ] # equivalent to above
    # enum([ :a , 10,  :b, 20 ])  # like C   enum { a=10; b=20; };
    #
    #   following define named enums (MRI FFI calls these 'tagged' enums)
    # enum :tfoo, [ :zero, :one, :two ] # like C   typedef enum { zero, one, two } tfoo;
    # enum :tbar  [ :a , 10,  :b, 20 ]  # like C   typedef enum { a=10; b=20; } tbar;
    #
    # Examples
    #    attach_function( :fcta, [ :int ], :void )
    #    fcta( :two ) #   :two will be looked up in the table of all enumerated values
    #
    #    attach_function( :fctb, [ :tfoo ], :tbar)
    #    fctb( :one ) #   :one will be looked up in the values for  tfoo
    #                 #  function result is translated from an int32 to a Symbol via tfoo
    #
    def enum(*args)
      arg_siz = args.size
      if arg_siz._equal?(1)
        return enum(nil, args[0])
      elsif arg_siz._equal?(2)
        a1 = args[0]
        a2 = args[1]
        if a1._isSymbol && a2._isArray
          return enum(a1, a2)
        end
      end
      enum(nil, args)
    end

    def enum( name, array)
      if name._not_equal?(nil)
        unless name._isSymbol ; raise TypeError, 'expected a Symbol'; end
      end
      unless array._isArray ; raise TypeError, 'expected an Array'; end
      e = Enum.new(array, name)
      Enums.add(e)
      if name._not_equal?(nil)
        typedef(e, name)
      end
    end

    def enum_type(name)
      if name._isString
        sym = name.to_sym
      elsif name._isSymbol
        sym = name
      else
        raise TypeError  , 'expected a String or Symbol'
      end
      Enums.__find_named_enum(sym)
    end

    def enum_value(symbol)
      unless symbol._isSymbol
        raise TypeError , 'expected a Symbol'
      end
      Enums.__enum_symbol_to_value(symbol)
    end

  end # ]
end
