 module FFI
   class MemoryPointer
    def inspect
      "#<FFI::MemoryPointer address=0x#{self.address.to_s(16)} size=#{self.size}>"
    end
    alias :to_i :address
   end

   class Pointer
    # a subclass of CByteArray , first opening in ffi.rb

    def self.new(type, count, &blk)
      # this variant  gets bridge methods
      if type._isFixnum
        elemsize = type
      elsif type._kind_of?(Struct.class)
        elemsize = type.size
      else
        modu =  FFI
        elemsize = modu.type_size(modu.find_type(type))
      end
      count = Type.coerce_to(count, Fixnum, :to_int)
      if (count < 1)
        raise ArgumentError , 'count must be a Fixnum >= 1'
      end
      numbytes = count * elemsize
      inst = self.__gc_malloc(numbytes)
      inst.initialize(elemsize)
      inst
      if block_given?
        yield(inst)
      else
        inst
      end
    end

    def self.new(type, &blk)
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

    NULL = self.new(0);

    # def initialize ; end #   in ffi.rb

    class_primitive_nobridge '__with_all', 'withAll:'

    def self.from_string(string)
      string = Type.coerce_to(string, String, :to_str)
      inst = self.__with_all(string)
      inst.__initialize(1)
      inst
    end

    def [](element_offset)
      elem_size = self.type_size
      byte_offset = element_offset * elem_size
      # self + byte_offset # old code
      self.__signed_wordsize_at(elem_size, byte_offset, nil)
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

    def each(&b)
      i = 0
      lim = self.size.__divide(self.type_size)
      while i < lim
        b.call(self[i])
        i += 1
      end
      self
    end

    def each_with_index(&b)
      i = 0
      lim = self.size.__divide(self.type_size)
      while i < lim
        b.call(self[i], i)
        i += 1
      end
      self
    end


    primitive_nobridge '__new_from', 'newFrom:numBytes:'

    def +(byteoffset)
      # return an instance derived from self
      #  instance has autofree==false, and a derivedFrom reference to self
      inst = self.__new_from(byteoffset, -2)
      inst.initialize(self.type_size)
      inst
    end

    def derived_from
      # the instance which owns the C memory pointed to by self,
      #  if nil, then the C memory is owned by self.
      # may be an Integer if created by read_pointer.
      @derivedFrom
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
      self.put_pointer(0, val);
    end

    # def put_pointer(ofs, memory_pointer) ; end
    primitive_nobridge 'put_pointer', 'pointerAt:put:'

    def get_pointer(ofs)
      self.__pointer_at(ofs, Pointer)
    end

    def read_string(num_bytes)
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

    def get_string(offset)
       zofs = self.__search_for_zerobyte(offset)
       lim = zofs < 0 ? self.size : zofs
       self.stringfrom_to(offset, lim - 1)  # both args zero based
    end
    def get_string(offset, length)
       zofs = self.__search_for_zerobyte(offset) # result zero based
       lim = zofs < 0 ? offset + length  : zofs
       if lim > (sz = self.size)
         lim = sz
       end
      self.stringfrom_to(offset, lim-1) # both args zero based
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

    def ==(pointer)
      unless pointer._kind_of?(Pointer) || pointer._kind_of?(CPointer)
        return false
      end
      pointer.address == self.address
    end

    def write_array_of_pointer(ary)
      # ary should be an Array of CByteArray's
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

    def get_array_of_string(byte_offset, length)
      unless byte_offset._isFixnum ; raise TypeError, 'offset must be a Fixnum'; end
      unless length._isFixnum ; raise TypeError, 'length must be a Fixnum'; end
      res = []
      n = 0
      limit = self.total
      if limit._equal?(0)
        while n < length
          ofs = byte_offset + (n << 3)
          str = self.char_star_at(ofs )
          res << str
          end
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

    def inspect
      "#<FFI::Pointer address=0x#{self.address.to_s(16)}>"
    end
    alias :to_i :address

    # following not yet used in rubyspecs
    # def read_array_of_type(type, reader, length) ; end # TODO
    # def write_array_of_type(type, writer, ary); ; end # TODO
    # def get_at_offset(offset, type) ; end # TODO
    # def set_at_offset(offset, type, val) ; end # TODO
  end
end
