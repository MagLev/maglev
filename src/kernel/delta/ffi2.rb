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
  end
  class CFunction
    primitive_nobridge 'call_template*' , '_rubyToCcallTemplate:'

    class_primitive_nobridge '__new', '_rubyNew:'
     # arg is [ cLibrary, fName, resType, argTypesArray, varArgsAfter]

    primitive_nobridge '__compile_caller', '_compileCaller:In:'
  end
  class CByteArray
    # following 3 methods needed by the RubyParser
    class_primitive_nobridge 'with_string', 'withAll:'
    primitive_nobridge '[]', '_rubyByteAt:'
    primitive_nobridge 'size', 'size'

    # following used for fields of Struct
    primitive_nobridge 'int16at', 'int16At:'
    primitive_nobridge 'int32at', 'int32At:'
    primitive_nobridge 'int64at', 'int64At:'
    primitive_nobridge 'int8at', 'int8At:'
    primitive_nobridge 'uint16at', 'uint16At:'
    primitive_nobridge 'uint32at', 'uint32At:'
    primitive_nobridge 'uint64at', 'uint64At:'
    primitive_nobridge 'uint8at', 'uint8At:'
    primitive_nobridge 'double_at', 'doubleAt:'
    primitive_nobridge 'double_put', 'doubleAt:put:'
    primitive_nobridge 'int16_put', 'int16At:put:'
    primitive_nobridge 'int32_put', 'int32At:put:'
    primitive_nobridge 'int64_put', 'int64At:put:'
    primitive_nobridge 'int8_put', 'int8At:put:'
    primitive_nobridge '__unsigned_wordsize_at', '_unsigned:at:'

    primitive_nobridge 'stringfrom_to', 'stringFrom:to:'
  # zero-based start offset,  zero-based end offset(NOT limit)

    primitive_nobridge 'copyfrom_from_to_into', 'copyFrom:from:to:into:'
       # object, one-based start, one-based end, zero-based dest offset

    class_primitive_nobridge 'gc_malloc' , 'gcMalloc:'
      # allocates C memory which is auto-freed when instance is GC'ed

    class_primitive_nobridge 'from_address', 'fromAddress:'

    primitive_nobridge 'address' , 'memoryAddress'

    primitive_nobridge 'memset' , 'memset:from:to:'
     # args are  ushort value, zero-based start offset,
     #    zero-based end offset (-1 means to end of allocated C memory)

    def _search_for_zerobyte(offset)
      # result -1 if no null byte found
      self.__unsigned_wordsize_at(-1, offset)
    end

    def self.new(size)
      # creates an instance with C data zeroed, and to be auto-freed by GC
      gc_malloc(size)
    end

    def clear
      self.memset(0, 0, -1)
    end

    primitive_nobridge '__inspect' , '_inspect'
    def inspect
      str = super
      str << self.__inspect
      str
    end

  end

  class CPointer
    primitive_nobridge 'address' , 'memoryAddress'
  end

  class Type
    def name
      @name
    end
    def size
      @size
    end
    def _prim_type
      @prim_type_name
    end
    def self._install_native_type(a_type)
      FFI.const_set( a_type.name , a_type )
    end
    def self._initialize_native_types
      _install_native_type( Type.new( :INT8, 1 , :int8))  # NativeType::INT8 in MRI implem
      _install_native_type( Type.new( :UINT8, 1 , :uint8))
      _install_native_type( Type.new( :INT16, 2 , :int16))
      _install_native_type( Type.new( :UINT16, 2 , :uint16))
      _install_native_type( Type.new( :INT32, 4 , :int32))
      _install_native_type( Type.new( :UINT32, 4 , :uint32))
      _install_native_type( Type.new( :INT64, 8 , :int64))
      _install_native_type( Type.new( :UINT64, 8 , :uint64))
      # FLOAT32 not supported yet
      _install_native_type( Type.new( :FLOAT64, 8 , :double))
      _install_native_type( Type.new( :VOID, 8 , :void))
      _install_native_type( Type.new( :STRING, 8 , :string))
      _install_native_type( Type.new( :BUFFER_INOUT, 8 , :ptr))
      _install_native_type( Type.new( :BUFFER_IN, 8 , :ptr))
      _install_native_type( Type.new( :BUFFER_OUT, 8 , :ptr))
      # ENUM, BOOL, CHAR_ARRAY, VARARGS  todo
    end
  end
  Type._initialize_native_types

  module_function() # [ ===================== following are module functions
    # these are after  'module_function' instead of inside of a 'class << self'
    # to avoid dynamic constant refs .

    def find_type(name)
      unless name._isSymbol
        raise TypeError , 'ruby_name must be a Symbol'
      end
#      code = PrimTypeDefs[name]
      code = if defined?(@ffi_typedefs) && @ffi_typedefs.has_key?(name)
               @ffi_typedefs[name]
             else
               PrimTypeDefs[name]
             end
      if code.equal?(nil)
        raise TypeError, "Unable to resolve FFI type '#{name}'"
      end
      return code
    end

    def type_size(type)
      size = PrimTypeSizes[type]
      if size.equal?(nil)
        unless type._isSymbol
          raise TypeError, "FFI::type_size - type argument must be a Symbol"
        else
          raise TypeError, "Unable to resolve FFI type '#{type}'"
        end
      end
      size
    end

    def size_to_type(size)
      if size.equal?(4)
        return :int
      elsif size.equal?(2)
        return :short
      elsif size.equal?(1)
        return :char
      elsif size.equal?(8)
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
    # of the locations, add FFI::USE_THIS_PROCESS_AS_LIBRARY.
    # The libraries are tried in the order given.
    #
    def ffi_lib(*names)
      len = names.length
      arr = []
      carr = []
      n = 0
      my_debug = FFI::DEBUG
      while n < len
        a_name = names[n]
        puts "--FFI:  ffi_lib: adding #{a_name}" if  my_debug > 0
        if a_name == USE_THIS_PROCESS_AS_LIBRARY
          carr[n] = nil
        else
          a_name = ::Type.coerce_to(names[n], String, :to_str)
          cl = CLibrary.named(a_name)
          carr[n] = cl
        end
        arr[n] = a_name
        n += 1
      end
      @ffi_libs = arr
      @ffi_clibs = carr
    end

    def ffi_libraries
      if defined?(@ffi_libs)
        @ffi_libs
      else
        []
      end
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
    def attach_function(name, a3, a4, a5=Undefined)
      name = ::Type.coerce_to(name, String, :to_s)
      if a5._not_equal?(Undefined)
        cname = ::Type.coerce_to(a3, String, :to_s)
        args = a4
        ret = a5
      else
        cname = name
        args = a3
        ret = a4
      end

      ffimod = FFI
      cargs = args.map { |t| ffimod.find_type(t) }
      ret = ffimod.find_type(ret)

      libs = @ffi_clibs
      if libs.equal?(nil)
        libs = [ nil ]
      end
      my_debug = DEBUG
      n = 0
      len = libs.length
      while n < len
        lib = libs[n]
        if lib.equal?(nil)
          puts "--FFI:  attach_function: #{cname} searching process"   if my_debug > 1
          found = CLibrary.__has_symbol(cname) # check entire process
          if found && my_debug > 0
             puts "--FFI:  attach_function: found #{cname} in process"
          end
        else
          puts "--FFI:  attach_function: #{cname} searching lib #{lib.name}"   if my_debug > 1
          found = lib.__has_symbol(cname)
          if found && my_debug > 0
             puts "--FFI:  attach_function: found #{cname} in lib #{lib.name}"
          end
        end
        if found
          cf = CFunction.__new([ lib, cname, ret, cargs, -1])
          meth = cf.__compile_caller(name, self)  # installs a method in self
    # which will be installed per Maglev.persistent_mode
          return meth
        end
        n += 1
      end
      raise FFI::NotFoundError, "Unable to find FFI '#{cname}' in: #{@ffi_lib}"
    end

    def typedef(atype, new_name)
      if defined?(@ffi_typedefs)
        ht = @ffi_typedefs
      else
        ht = Hash.new
        @ffi_typedefs = ht
      end
      unless new_name._isSymbol
        raise TypeError , 'name must be a Symbol'
      end
      tcls = FFI::Type
      code = if atype.kind_of?(FFI::Type)
               atype
      elsif atype == :enum
        if new_name._isArray
          self.enum(new_name)
        else
          self.enum(info, new_name)
        end
      else
        @ffi_typedefs[atype] || FFI.find_type(atype)
      end

      @ffi_typedefs[new_name] = code
    end

    def enum(*args)
      # example
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
      name = nil
      arr = args
      if args.size.equal?(2)
        name = args[0]
        arr =  args[1]
        unless name._isSymbol && arr._isArray
          name = nil
          arr = args
        end
      end
      e = Enum.new(arr, name)
      Enums.add(e)
      if name._not_equal?(nil)
        typedef(e, name)
      end
      e
    end

    def enum_type(name)
      unless defined?(@ffi_enums)
        return nil
      end
      @ffi_enums.find(name)
    end

    def enum_value(symbol)
      @ffi_all_enum_vals[symbol]
    end

  end #]
end
