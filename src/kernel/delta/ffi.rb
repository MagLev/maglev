module FFI

  # The smalltalk implementation classes
  CLibrary = _resolve_smalltalk_global( :CLibrary )
  CFunction = _resolve_smalltalk_global( :CFunction )
  CByteArray = _resolve_smalltalk_global( :CByteArray )
  CPointer = _resolve_smalltalk_global( :CPointer )

  #  Specialised error classes
  class TypeError < RuntimeError; end

  class NotFoundError < RuntimeError; end

  class NativeError < LoadError; end

  class PlatformError < FFI::NativeError; end

  class SignatureError < NativeError; end

  USE_THIS_PROCESS_AS_LIBRARY = nil

  # DEBUG , a dynamic constant in post_prims/ffi.rb  

  module Platform

    OS = "" # you must use Config::CONFIG['host_os']
	    # because OS could change after you commit code

    ARCH = ""   # arch should be determined dynamically 
    LIBPREFIX = "lib"
    LIBSUFFIX = "" # OS  dependent suffix appended at runtime by the
                   # library load primitives if '.' not in lib name, 
                   #    or if '.' is last character of lib name. 
    LONG_SIZE = 64 # in bits
    ADDRESS_SIZE = 64
  end

  # tables used to translate arguments for primitives.
  # mapping from Ruby type names to type names supported by CFunction
  PrimTypeDefs = IdentityHash.from_hash( 
    { :char => :int8 ,  :uchar => :uint8 ,
      :short => :int16 , :ushort => :uint16 ,
      :int   => :int32 , :uint => :uint32 ,
      :long  => :int64 , :ulong => :uint64 ,
      :int8 => :int8,  :uint8 => :uint8 ,
      :int16 => :int16 , :uint16 => :uint16 ,
      :int32 => :int32 , :uint32 => :uint32 ,
      :int64 => :int64 , :uint64 => :uint64,
      :size_t => :uint64 , 
      :long_long => :int64 ,
      :ulong_long => :uint64 ,
#      :float  =>  :double ,  # not supported yet by CFunction primitives
      :double =>  :double ,
      :pointer => :ptr ,
      :void => :void ,
      :string => :'char*' ,
      :const_string => :'const char*'} )

# Body of a  :const_string  argument is copied from object memory to C
#   memory before a function call .
# Body of a  :string  argument is copied from object memory to C memory
#   before a function call, and is then copied back to object memory
#   after the function call. 
    
  # mapping from types supported by CFunction to sizes in bytes
  PrimTypeSizes = IdentityHash.from_hash( 
    { :int8 => 1,  :uint8 => 1,
      :int16 => 2 , :uint16 => 2 ,
      :int32 => 4 , :uint32 => 4 ,
      :int64 => 8 , :uint64 => 8,
      :double =>  8 ,
      :ptr => 8 ,
      :void => 8 ,
      :'char*' => 8,
      :'const char*' => 8 } )

  StructAccessors = IdentityHash.from_hash( 
    # values are selectors for use with __perform_se
    { :char =>  :'uint8at:' ,  :uchar => :'uint8at:' ,
      :short => :'int16at:' , :ushort => :'uint16at:' ,
      :int   => :'int32at:' , :uint => :'uint32at:' ,
      :long  => :'int64at:' , :ulong => :'uint64at:' ,
      :int8 =>  :'int8at:',  :uint8 => :'uint8at:' ,
      :int16 => :'int16at:' , :uint16 => :'uint16at:' ,
      :int32 => :'int32at:' , :uint32 => :'uint32at:' ,
      :int64 => :'int64at:' , :uint64 => :'uint64at:' ,
      :size_t => :'uint64at:' ,
      :long_long => :'int64at:' ,
      :ulong_long => :'uint64at:' ,
  #    :float  =>  :double ,  
      :double =>  :'double_at:' ,
  #   :pointer => :ptr ,
  #   :void => :void ,
  #   :string => :'char*' ,
  #   :const_string => :'const char*' 
       } )
    
  StructSetters = IdentityHash.from_hash( 
   # values are selectors for use with __perform__se
    { :char => :'int8_put::' ,  :uchar => :'int8_put::' ,
      :short => :'int16_put::' , :ushort => :'int16_put::' ,
      :int   => :'int32_put::' , :uint => :'int32_put::' ,
      :long  => :'int64_put::' , :ulong => :'int64_put::' ,
      :int8 => :'int8_put::',  :uint8 => :'int8_put::' ,
      :int16 => :'int16_put::' , :uint16 => :'int16_put::' ,
      :int32 => :'int32_put::' , :uint32 => :'int32_put::' ,
      :int64 => :'int64_put::' , :uint64 => :'int64_put::' ,
      :size_t => :'int64_put::' ,
      :long_long => :'int64_put::' ,
      :ulong_long => :'int64_put::' ,
  #    :float  =>  :double ,  
      :double =>  :'double_put::' ,
  #   :pointer => :ptr ,
  #   :void => :void ,
  #   :string => :'char*' ,
  #   :const_string => :'const char*' 
       } )

  class Enums 
    def initialize
      raise 'instances of Enums not used yet'
    end

    # remainder in ffi_enum.rb
  end

  class Enum
    def name
      @name
    end
    def tag
      @name
    end
    def symbol_map
      @kv_map
    end
    alias to_h symbol_map

    def _printable_name
      n = @name
      if name.equal?(nil)
        'unnamed'
      else
        n.to_s
      end
    end

    def _vk_map
      @vk_map
    end

    # remainder in ffi_enum.rb
  end

  class MemoryPointer < CByteArray
    # define the fixed instvars 
    def initialize
      @type_size = 1
    end
    def initialize(elem_size)
      unless elem_size._isFixnum && elem_size > 0
        raise TypeError, 'element size must be a Fixnum > 0'
      end
      @type_size = elem_size
    end
    def type_size
      @type_size
    end
    def _type_size=(elem_size)
      unless elem_size._isFixnum && elem_size > 0
        raise TypeError, 'element size must be a Fixnum > 0'
      end
      @type_size = elem_size 
    end
     
    # remainder of implementation in memorypointer.rb
  end

  # subclasses of MemoryPointer
  class AutoPointer < MemoryPointer
    # All Maglev MemoryPointer's have auto-free behavior unless
    #  auto-free explicitly disabled on an instance
  end
  class Buffer < MemoryPointer
    # remainder of implementation in buffer.rb
  end

  class Struct
    # define the fixed instvars 
    def initialize
      ly =  self.class._layout
      @layout = ly
      @bytearray = CByteArray.gc_malloc(ly.size)
    end
 
    # remainder of implementation in ffi_struct.rb
  end

  class StructLayout
    # define the fixed instvars 
    def initialize
      @members_dict = IdentityHash.new # keys are Symbols, 
				       #  values are offsets into @members...@setters
      @members = []
      @offsets = []
      @sizes = []
      @accessors = []
      @setters = []
      @totalsize = 0
      @closed = false
    end

    # remainder of implementation in ffi_struct.rb
  end

  class Type
    def initialize(a_sym, a_size, pt_sym)
      @name = a_sym
      @size = a_size # in bytes
      @prim_type_name = pt_sym # a name known to CFunction, or name of another Type
    end
  end

end

FFI.__freeze_constants

# the rest of the FFI implementation is in file ffi2.rb
