module FFI

  # Maglev does not yet support:
  #    FFI::Union
  #    FFI::Library.callback

  # Smalltalk implementation classes
  class CLibrary
     class_primitive_nobridge 'named' , 'named:'
     class_primitive_nobridge '_has_symbol', 'hasCSymbol:'
     primitive_nobridge '_has_symbol', 'hasCSymbol:'
  end
  class CFunction
    primitive_nobridge 'call_template*' , '_rubyToCcallTemplate:'

    class_primitive_nobridge '_new', '_rubyNew:'
     # arg is [ cLibrary, fName, resType, argTypesArray, varArgsAfter]

    primitive_nobridge '_compile_caller', '_compileCaller:In:'
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

    class_primitive_nobridge 'gc_malloc' , 'gcMalloc:'
      # allocates C memory which is auto-freed when instance is GC'ed

    primitive_nobridge 'address' , 'memoryAddress'

    primitive_nobridge 'memset' , 'memset:from:to:' 
     # args are  ushort value, zero-based start offset, 
     #    zero-based end offset (-1 means to end of allocated C memory)

    def clear
      self.memset(0, 0, -1)
    end

  end

  class CPointer
    primitive_nobridge 'address' , 'memoryAddress'
  end

  class << self

    def find_type(name)
      unless name._isSymbol
        raise TypeError , 'ruby_name must be a Symbol'
      end
      code = TypeDefs[name]
      if code.equal?(nil)
        raise TypeError, "Unable to resolve FFI type '#{name}'"
      end
      return code
    end

    def type_size(type)
      sz = TypeSizes[type]
      if size.equal?(nil)
        unless type._isSymbol
          raise TypeError, "FFI::type_size - type argument must be a Symbol"
        else
          raise TypeError, "Unable to resolve FFI type '#{type}'"
        end
      end
      sz
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
  end

  module Library
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
      while n < len
        a_name = names[n]
        if a_name == USE_THIS_PROCESS_AS_LIBRARY
          carr[n] = nil
        else
          a_name = Type.coerce_to(names[n], String, :to_str)
          carr[n] = CLibrary.named(a_name)
        end
        arr[n] = a_name
        n += 1
      end
      @ffi_lib = arr
      @ffi_clibs = carr
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
      name = Type.coerce_to(name, String, :to_s)
      if a5._not_equal?(Undefined)
        cname = Type.coerce_to(a3, String, :to_s)
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
      n = 0
      len = libs.length
      while n < len
        lib = libs[n]
        if lib.equal?(nil)
          found = CLibrary._has_symbol(cname) # check entire process
        else
          found = lib._has_symbol(cname)
        end
        if found
          cf = CFunction._new([ lib, cname, ret, cargs, -1])
          meth = cf._compile_caller(name, self)
          return meth
        end
        n += 1
      end
      raise FFI::NotFoundError, "Unable to find FFI '#{cname}' in: #{@ffi_lib}"
    end

  end
end
