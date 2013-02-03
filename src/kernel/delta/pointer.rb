module FFI
  # class MemoryPointer < Pointer ... end     in ffi2.rb
  class MemoryPointer
    def inspect
      "#<FFI::MemoryPointer #{self.__inspect}>"
    end
    alias :to_i :address
  end


  # Pointer is the base class for a number of other types in the FFI
  # library (MemoryPointer, Struct, etc.).
  #
  # +Pointer+ is identically Smalltalk +CByteArray+.
  #
  # Pointer (CByteArray) encapsulates allocation of C memory or a reference
  # to C memory.
  #
  # A pointer maintains a flag that controls whether <tt>free()</tt> is
  # called when the pointer object is garbage collected by the VM.
  #
  # Pointers know the type of object they point to, and the size of the
  # memory (number of elements).
  #
  # == Derived Pointers.
  #
  # A pointer may be "derived from" another pointer, which means that it
  # points to memory managed by the parent pointer (e.g., a base pointer
  # points to a struct, and another pointer, derived from the base, points
  # to one field in that struct).  Each derived pointer maintains a
  # reference to the base pointer to keep memory pinned from garbage
  # collection.
  #
  class Pointer # [

    # call-seq:
    #   Pointer.new              {|ptr| block} -> ptr to eight bytes of memory
    #   Pointer.new(num_bytes)   {|ptr| block} -> ptr to +num_bytes+ bytes
    #   Pointer.new(type)        {|ptr| block} -> ptr for one element of +type+
    #   Pointer.new(type, count) {|ptr| block} -> ptr for +count+ elements of +type+
    #
    # Create and return a new Pointer.  The pointer will allocate and
    # manage enough memory on the C-heap to hold +count+ instances of
    # +type+.  +type+ is one of the FFI::PrimTypeDefs in <tt>ffi2.rb</tt>
    # (e.g., +:char+, +:int32+, +:double+, +:string+, +:pointer+, etc.).
    #
    # If called with no parameters, returns a pointer large enough to hold
    # a pointer, or an int64 (8 bytes).  If called with one parameter, and
    # a count, then space for that number of elements of type is allocated.
    #
    # If a block is passed, then the newly created pointer is yielded to
    # the block.
    #
    # Writing nil as a pointer's value creates a A C NULL pointer:
    #
    #   ptr = FFI::MemoryPointer.new(...)
    #   ptr.put_pointer(0, nil)
    #   ptr.null?   # => true
    #
    # == Example
    #
    # Create a pointer to a long and then write a value to the memory
    # manged by the pointer and then read it back:
    #
    #   long_ptr = FFI::MemoryPointer.new :long
    #   long_ptr.put_long(0, 0x12345678)
    #   ruby_value = long_ptr.read_long
    #
    # Create a pointer to an array of 100 ints:
    #
    #   one_hundred_ints = FFI::Pointer.new(:int, 100)
    #
    #

    def self.new(type, count=MaglevUndefined, &block)
      # this variant  gets bridge methods
      uu = MaglevUndefined
      if count._equal?(uu)
        if type._equal?(uu)
          return self.new()
        else
          return self.new(type, &block)
        end
      end
      if type._isFixnum
        elemsize = type
      elsif type._kind_of?(Struct.class)
        elemsize = type.size
      else
        modu =  FFI
        elemsize = modu.type_size(modu.find_type(type))
      end
      count = Maglev::Type.coerce_to(count, Fixnum, :to_int)
      if (count < 0)
        raise ArgumentError , 'count must be a Fixnum >= 0'
      end
      numbytes = count * elemsize
      inst = self.__gc_malloc(numbytes)
      inst.initialize(elemsize)
      if block_given?
        yield(inst)
      else
        inst
      end
    end

    def self.new(type, &block)
      inst = self.new(type)
      if block_given?
        yield(inst)
      else
        inst
      end
    end

    def self.new(type)
      if type._isFixnum
        if type._equal?(0)
          return self.__new_null # the NULL pointer
        end
        numbytes = type
        inst = self.__gc_malloc(numbytes)
        inst.initialize(1)
      else
        if type._kind_of?(Struct.class)
          elemsize = type.size
        else
          modu =  FFI
          elemsize = modu.type_size(modu.find_type(type))
        end
        inst = self.__gc_malloc(elemsize)
        inst.initialize(elemsize)
      end
      inst
    end

    def self.new
      # result can hold a single C pointer or int64
      inst = self.__gc_malloc(8)
      inst.initialize(1)
      inst
    end

    NULL = self.new(0)

    def initialize(elem_size=MaglevUndefined)
      if elem_size._isFixnum && elem_size > 0
        @_st_typeSize = elem_size
      elsif elem_size._equal?(MaglevUndefined)
        @_st_typeSize = 1
      else
        raise TypeError, 'element size must be a Fixnum > 0'
      end
    end
    def initialize
      @_st_typeSize = 1
    end
    def __initialize(elem_size)
      @_st_typeSize = elem_size
    end

    def type_size
      @_st_typeSize
    end

    def __type_size=(elem_size)
      unless elem_size._isFixnum && elem_size > 0
        raise TypeError, 'element size must be a Fixnum > 0'
      end
      @_st_typeSize = elem_size
    end


    # call-seq:
    #   __with_all(an_object) -> Pointer
    #
    # +an_object+ may be a String, ByteArray or CByteArray.  Returns a new
    # instance of the receiver containing a copy of the bytes of the
    # argument.  If anObject is a String, the resulting ByteArray contains
    # the bytes of the String plus a terminator byte with value zero.
    class_primitive_nobridge '__with_all', 'withAll:'

    # Create a new Pointer that manages a copy of the string on the
    # C-Heap. The C-memory will have an extra byte with a value of zero
    # (i.e., the c-string is null terminated).
    #
    #   my_ptr = Pointer.from_string("hello")
    #
    def self.from_string(string)
      string = Maglev::Type.coerce_to(string, String, :to_str)
      inst = self.__with_all(string)
      inst.__initialize(1)
      inst
    end

    # Returns the nth element pointed to by receiver. The number of bytes
    # returned is controlled by the type of this pointer (:int, :double,
    # etc.).  Roughly, return element type number of bytes starting at:
    #
    #     pointer_base + (element_offset * element_size)
    def [](element_offset)
      elem_size = self.type_size
      byte_offset = element_offset * elem_size
      # self + byte_offset # old code
      self.__signed_wordsize_at(elem_size, byte_offset, nil)
    end

    def +(byteoffset)
      # return an instance derived from self
      #  instance has autofree==false, and a derivedFrom reference to self
      inst = self.__new_from(byteoffset, -2)
      inst.initialize(self.type_size)
      inst
    end

    primitive_nobridge '__new_from', 'newFrom:numBytes:'

    def ==(pointer)
      unless pointer._kind_of?(Pointer) || pointer._kind_of?(CPointer)
        return false
      end
      pointer.address == self.address
    end

    def derived_from
      # the instance which owns the C memory pointed to by self,
      #  if nil, then the C memory is owned by self.
      # may be an Integer if created by read_pointer.
      @_st_derivedFrom
    end

    def each(&block)
      unless block_given?
        return self.to_a.each()  # added for 1.8.7
      end
      i = 0
      lim = self.size.__divide(self.type_size)
      while i < lim
        block.call(self[i])
        i += 1
      end
      self
    end

    def each_with_index(&block)
      unless block_given?
        return self.to_a.each_with_index()  # added for 1.8.7
      end
      i = 0
      lim = self.size.__divide(self.type_size)
      while i < lim
        block.call(self[i], i)
        i += 1
      end
      self
    end

    def get_pointer(ofs)
      self.__pointer_at(ofs, Pointer)
    end

    def inspect
      "#<FFI::Pointer #{self.__inspect}>"
    end

    def read_int
      self.get_int(0)
    end
    def write_int(val)
      self.put_int(0, val)
    end
    def read_long
      self.int64at(0)
    end
    def write_long(val)
      self.int64_put(0, val)
    end
    def write_double(val)
      self.double_put(0, val)
    end
    def read_double(val)
      self.double_at(0)
    end
    def read_pointer
      self.__pointer_at(0, Pointer) # returns a derived pointer
    end
    def write_pointer(val)
      self.put_pointer(0, val)
    end

    # def put_pointer(ofs, memory_pointer) ; end
    primitive_nobridge 'put_pointer', 'pointerAt:put:'

    def read_string(num_bytes=MaglevUndefined)
      if num_bytes._equal?(MaglevUndefined)
        return self.read_string()
      end
      self.stringfrom_to(0, num_bytes - 1)
    end
    def read_string
      zofs = self.__search_for_zerobyte(0)
      lim = zofs < 0 ? self.total : zofs
      self.stringfrom_to(0, lim - 1)
    end
    def write_string(string, num_bytes)
      self.copyfrom_from_to_into(string, 1, num_bytes, 0)
    end

    def get_string(offset, length=MaglevUndefined)
      if length._equal?(MaglevUndefined)
        return self.get_string(offset)
      end
      zofs = self.__search_for_zerobyte(offset) # result zero based
      lim = zofs < 0 ? offset + length  : zofs
      if lim > (sz = self.size)
        lim = sz
      end
      self.stringfrom_to(offset, lim-1) # both args zero based
    end

    def get_string(offset)
      zofs = self.__search_for_zerobyte(offset)
      lim = zofs < 0 ? self.size : zofs
      self.stringfrom_to(offset, lim - 1)  # both args zero based
    end

    def put_string(offset, string)
      len = string.length
      self.copyfrom_from_to_into(string, 1, len, offset)
      # obj, one-based, one-base, zero-based
      self.int8_put(offset + len, 0)  # add a null byte
    end

    def null?
      self.address._equal?(0)
    end

    def read_array_of_int(length)
      unless length._isFixnum ; raise TypeError, 'length must be a Fixnum'; end
      length = length.__min(self.total.__divide(4))
      res = Array.new(length)
      n = 0
      while n < length
        res[n] = self.get_int(n << 2)
        n += 1
      end
      res
    end

    def read_array_of_long(length)
      unless length._isFixnum ; raise TypeError, 'length must be a Fixnum'; end
      length = length.__min(self.total.__divide(8))
      res = Array.new(length)
      n = 0
      while n < length
        res[n] = self.int64at(n << 3)
        n += 1
      end
      res
    end

    def write_array_of_int(ary)
      unless ary._isArray ; raise TypeError, 'expected an Array'; end
      len = ary.length
      if len > self.total.__divide(4)
        raise ArgumentError, 'arg has too many elements for receiver'
      end
      n = 0
      while n < len
        self.put_int(n << 2, ary[n])
        n += 1
      end
      self
    end

    def write_array_of_long(ary)
      unless ary._isArray ; raise TypeError, 'expected an Array'; end
      len = ary.length
      if len > self.total.__divide(8)
        raise ArgumentError, 'arg has too many elements for receiver'
      end
      n = 0
      while n < len
        self.int64_put(n << 3, ary[n])
        n += 1
      end
      self
    end

    def write_array_of_pointer(ary)
      # ary should be an Array of Pointer's
      unless ary._isArray ; raise TypeError, 'expected an Array'; end
      len = ary.length
      if len > self.total.__divide(8)
        raise ArgumentError, 'arg has too many elements for receiver'
      end
      n = 0
      while n < len
        self.__pointer_at_put( n << 3 , ary[n] )
        n += 1
      end
    end

    def read_array_of_pointer(length)
      unless length._isFixnum ; raise TypeError, 'length must be a Fixnum'; end
      res = Array.new(length)
      length = length.__min(self.total.__divide(8))
      n = 0
      while n < length
        res[n] = self.__pointer_at(n << 3, Pointer)
        n += 1
      end
      res
    end

    def get_array_of_string(byte_offset, length=MaglevUndefined)
      unless length._isFixnum
        if length._equal?(MaglevUndefined)
          return self.get_array_of_string(byte_offset)
        end
        raise TypeError, 'length must be a Fixnum'
      end
      unless byte_offset._isFixnum ; raise TypeError, 'offset must be a Fixnum'; end
      res = []
      n = 0
      limit = self.total
      if limit._equal?(0)
        while n < length
          ofs = byte_offset + (n << 3)
          str = self.char_star_at(ofs )
          res << str
          n += 1
        end
      else
        while n < length
          ofs = byte_offset + (n << 3)
          if ofs >= limit
            raise IndexError, "beyond end of receiver's C memory"
          else
            str = self.char_star_at(ofs )
            res << str
          end
          n += 1
        end
      end
      res
    end

    def get_array_of_string(byte_offset)
      unless byte_offset._isFixnum ; raise TypeError, 'offset must be a Fixnum'; end
      res = []
      ofs = byte_offset
      limit = self.total
      while ofs < limit
        str = self.char_star_at(ofs)
        if str._equal?(nil)
          return res
        end
        res << str
        ofs += 8
      end
      res
    end

    def to_a
      elem_size = self.type_size
      siz = self.total
      ofs = 0
      res_size = siz.__divide(elem_size)
      ary = Array.new(res_size)
      n = 0
      while ofs < siz
        ary[n] = self.__signed_wordsize_at(elem_size, ofs, nil)
        ofs += elem_size
        n += 1
      end
      ary
    end

    alias :to_i :address

    def to_ptr
      self
    end

    # following not yet used in rubyspecs
    # def read_array_of_type(type, reader, length) ; end # TODO
    # def write_array_of_type(type, writer, ary); ; end # TODO
    # def get_at_offset(offset, type) ; end # TODO
    # def set_at_offset(offset, type, val) ; end # TODO
  end # ]
end
