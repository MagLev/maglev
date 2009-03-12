module FFI
  # intended to be somewhat API compatible with the Rubinius implementation

  #  Specialised error classes

  USE_THIS_PROCESS_AS_LIBRARY = nil  
  
  class CLibrary
    class_primitive_nobridge 'named', 'named:'
    primitive_nobridge 'name', 'name'
  end 
 
  class CFunction 
    class_primitive '_create', '_rubyNew:'

    def self.create(c_library, cname, restype, argtypes_array, num_var_args=-1)
      arr = [ c_library, cname, restype, argtypes_array, num_var_args]
      self._create(arr) 
    end

    primitive '_call', 'callWith:'
  end 

  class CByteArray
    primitive 'gc_malloc', 'gcMalloc:'
    def self.new(size)
      # creates an instance with C data zeroed, and to be auto-freed by GC
      gc_malloc(size)
    end
  end

  MemoryPointer = CByteArray 
 
  RubyToStArgTypes = IdentityHash.from_hash(
    # keys are allowed values for elements of the arguments specification
    #   argument to the attach_function method.
    { 	:char =>  :int8 , 
	:uchar => :uint8 ,
	:short => :int16 ,
	:ushort => :uint16 ,
	:int    => :int32 ,
	:uint   => :uint32 ,
	:long   => :int64 ,
	:ulong   =>:uint64 ,
	:intptr_t => :int64 ,
	:uintptr_t => :uint64 ,
	:ssize_t => :int64 ,
	:size_t => :uint64 ,
	:float      => :double ,
	:double     => :double ,
	:pointer    => :ptr  ,
	:void       => :void ,
	:string     => ('char*'.to_sym ) ,   # copied from Object to C memory before
				# C call and copied from C memory to Object after
	:const_string => 'const char*'.to_sym   
 				# copied from Object to C memory before C call
	# :strptr     
        #  :char_array     # to describe a struct, needs a size also
    } )

  RubyToStResultTypes = IdentityHash.from_hash(
    # keys are allowed values for return type argument to the attach_function method.
  {   :char =>  :int8 ,
        :uchar => :uint8 ,
        :short => :int16 ,
        :ushort => :uint16 ,
        :int    => :int32 ,
        :uint   => :uint32 ,
        :long   => :int64 ,
        :ulong   =>:uint64 ,
	:intptr_t => :int64 ,
	:uintptr_t => :uint64 ,
	:ssize_t => :int64 ,
	:size_t => :uint64 ,
        :float      => :double ,
        :double     => :double ,
        :pointer    => :ptr  ,
        :void       => :void ,
        :string     => 'char*'.to_sym   # result becomes a String object
    } )

  class Library 

    def self.ffi_lib(*names)
      # create a new subclass describing the specified shared library
      sub_lib = Class.new(Library);
      sub_lib.setup(names)
      sub_lib
    end

    def self.ffi_lib()
      # create a new subclass describing some functions in the current process
      self.ffi_lib([])  # use libraries loaded in current process
    end

    def self.setup(*names)
      # the primary subclass initialization method
      if names.size > 1 
        raise ArgumentError, 'only zero or one shared library may be specified'
      end 
      if names.size == 1
        fullname = 'lib' + names[0] + '.so' 
        @@ffi_lib = CLibrary.named(fullname)
      else
        @@ffi_lib = USE_THIS_PROCESS_AS_LIBRARY
      end
      @@cfunctions_table = [] 
    end


    def self.ffi_library_names
      if @@ffi_lib == USE_THIS_PROCESS_AS_LIBRARY 
        return [ 'this_process' ]
      else
        return [ @@ffi_lib.name ] 
      end 
    end
    # def self.setup_ld_library_path(library); end # not implemented


    # Attach a C function to this subclass of Library. The arguments can have two forms:
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
    #
    def self.attach_function(name, a3, a4, a5=nil)
      if a5
        name = a3.to_s
        args = a4
        ret = a5
      else
        name = name.to_s
        args = a3
        ret = a4
      end
      mname = name.to_sym

      st_args = args.map { | k | 
          v = RubyToStArgTypes[k]
          if v.equal?(nil)
            raise ArgumentError, "unsupported C argument type #{k}"
          end 
          v
        }
      st_ret = RubyToStResultTypes[ret]
      if st_ret.equal?(nil) 
        raise ArgumentError, "unsupported C result type #{ret}"
      end
     
      cfunct = CFunction.create(self, name, st_ret , st_args) 
      ofs = @@cfunctions_table.size      
      ofs_str = ofs.to_s
      @@cfunctions_table[ofs] = cfunct
      src = "def #{mname}(*args) \n  @cfunctions_table[#{ofs_str}]._call(*args) \n end"
      self.module_eval( src )  
    end

    def initialize
      # instance initialization, copy the functions table
      @cfunctions_table = @@cfunctions_table
    end
  end 


  class Struct < CByteArray 
    primitive_nobridge 'int8at' ,  'int8At:'
    primitive_nobridge 'int16at'  ,  'int16At:'
    primitive_nobridge 'int32at' , 'int32At:'
    primitive_nobridge 'int64at' , 'int64At:'
    primitive_nobridge 'uint8at' ,  'uint8At:'
    primitive_nobridge 'uint16at' ,  'uint16At:'
    primitive_nobridge 'uint32at' , 'uint32At:'
    primitive_nobridge 'uint64at' , 'uint64At:'
    primitive_nobridge 'doubleat' , 'doubleAt:'

    primitive_nobridge 'int8put' ,  'int8At:put:'
    primitive_nobridge 'int16put' ,  'int16At:put:'
    primitive_nobridge 'int32put' , 'int32At:put:'
    primitive_nobridge 'int64put' , 'int64At:put:'
    primitive_nobridge 'doubleput' , 'doubleAt:put:'
    primitive_nobridge 'pointerput' , 'pointerAt:put:'

    ScalarTypes = IdentityHash.from_hash(
    {   # keys are allowed field types
        # values  [ size, getter, setter ,  ]
     	:char => [ 1, 'int8at' , 'int8put' ] ,
	:uchar => [ 1, 'uint8at' , 'int8put' ],
	:short => [ 2 , 'int16at' , 'int16put' ],
	:ushort => [ 2 , 'uint16at' , 'int16put' ],
	:int => [ 4 , 'int32at' , 'int32put' ],
	:uint => [ 4 , 'uint32at' , 'int32put' ],
	:long => [ 8 , 'int64at' , 'int64put' ],
	:ulong => [ 8 , 'uint64at' , 'int64put' ],
	:intptr_t => [ 8 , 'int64at' , 'int64put' ],
	:uintptr_t => [ 8 , 'uint64at' , 'int64put' ],
	:ssize_t => [ 8 , 'int64at' , 'int64put' ],
	:size_t => [ 8 , 'uint64at' , 'int64put' ],
	:double     => [ 8 , 'doubleat' , 'doubleput' ],
	:pointer    => [ 8 , nil , 'pointerput' ],
        :pad1  => [ 1, nil, nil],
        :pad2  => [ 2, nil, nil],
        :pad3  => [ 3, nil, nil],
        :pad4  => [ 4, nil, nil],
        :pad5  => [ 5, nil, nil],
        :pad6  => [ 6, nil, nil],
        :pad7  => [ 7, nil, nil]
      #TODO  :const_pointer , :const_strptr, :strptr, :char_array
    } )

    def self.new_struct(fields)
      sub_cls = Class.new(Struct)
      sub_cls.setup(fields)
      sub_cls 
    end
      
    def self.setup(fields)
      # fields is of the form [ [ scalar_field_name, type ],... ,
      #                         [ array_field_name, type, num_elements ] ... ]
      # ordered by offset within the field
      #
      #  a name of __pad  is used to specify padding bytes
      # if size_bytes is <= 0, it will be computed from the specified fields

      offset = 0
      fields.each { | fspec |
        name = fspec[0] 
        unless name._isSymbol ; raise TypeError,'expected a Symbol' ; end
        type = fspec[1] 
        info = ScalarTypes[type] 
        if info.equal?(nil)
           raise ArgumentError, "unrecognized field type #{type}"
        end
        fspeclen = fspec.length
        if fspeclen == 2
          
          offset += info[0]
        elsif fspeclen == 3
          raise ArgumentError,'not implemented'
        else
          raise ArgumentError, 'element of fields must be of size 2 or 3'
        end
        getter = info[1]
        setter = info[2]

        unless getter.equal?(nil)
          src = "def #{name}(ofs) \n self.#{getter}(ofs) \n end "
          self.module_eval(src)
        end
        unless setter.equal?(nil)
          src = "def #{name}=(ofs, val) \n self.#{setter}(ofs, val) \n end "
          self.module_eval(src)
        end
      }
      total_size = offset
      src = "def self.new() \n self.gc_malloc(#{total_size}) \n end "
      self.module_eval(src)
    end
  end 

end

