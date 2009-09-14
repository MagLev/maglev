 module FFI
   class MemoryPointer  
    # a subclass of CByteArray , first opening in ffi.rb

    def self.new(type, count, &blk)
      if type._isFixnum
        elemsize = type
      else
        elemsize = FFI.type_size(type)
      end
      count = Type.coerce_to(count, Fixnum, :to_int)
      if (count < 1)
        raise ArgumentError , 'count must be a Fixnum >= 1'
      end
      numbytes = count * elemsize
      inst = self.gc_malloc(numbytes)
      inst.initialize(elemsize)
      inst
      if block_given?
        yield(inst)
      else
        inst
      end
    end
      
    def self.new(arg, &blk)
      if arg._isFixnum
        numbytes = arg
        inst = self.gc_malloc(numbytes)
        inst.initialize(1)
      else
        elemsize = FFI.type_size(type)
        inst = self.gc_malloc(elemsize)
        inst.initialize(elemsize)
      end
      if block_given?
        yield(inst)
      else
        inst
      end
    end

    class_primitive_nobridge '_with_all', 'withAll:'

    def self.from_string(string)
      string = Type.coerce_to(string, String, :to_str)
      inst = self._with_all(string)
      inst.initialize(1)
      inst
    end

    def [](element_offset)
      elem_size = @type_size
      byte_offset = element_offset * elem_size
      self + byte_offset
    end

    primitive_nobridge '_new_from', 'newFrom:numBytes:'

    def +(byteoffset)
      # return an instance derived from self
      #  instance has autofree==false, and a derivedFrom reference to self
      inst = self._new_from(byteoffset, -2) 
      inst.initialize(@type_size)
      inst
    end

    primitive_nobridge 'autorelease', 'autoRelease'

    # def autorelease=(val) ; end # Maglev TODO 

    def derived_from
      # the instance which owns the C memory pointed to by self,
      #  if nil, then the C memory is owned by self.
      @derivedFrom
    end

    def read_int
      self.int32at(0)
    end
    def write_int(val)
      self.int32_put(0, val)
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
    # def read_pointer ; end #  TODO
    # def write_pointer ; end #  TODO
    # def address ; end #  TODO
    # def null? ; end #  TODO

    def read_array_of_int(a_length)
      res = Array.new(a_length)
      n = 0
      while n < a_length 
        res[n] = self.int32at(n)
        n += 1
      end
      res
    end

    def read_array_of_long(a_length)
      res = Array.new(a_length)
      n = 0
      while n < a_length 
        res[n] = self.int64at(n)
        n += 1
      end
      res
    end

    def write_array_of_int(ary)
      len = ary.length
      n = 0
      while n < a_length
        self.int32_put(n, ary[n])
        n += 1
      end
      res
    end
    
    def write_array_of_int(ary)
      len = ary.length
      n = 0
      while n < a_length
        self.int64_put(n, ary[n])
        n += 1
      end
      res
    end
   
    # def read_array_of_type(type, reader, length) ; end # TODO
    # def write_array_of_type(type, writer, ary); ; end # TODO
    # def get_at_offset(offset, type) ; end # TODO
    # def set_at_offset(offset, type, val) ; end # TODO
  end
end
