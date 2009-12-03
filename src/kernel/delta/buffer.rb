module FFI

  class Buffer
    def self.alloc_in(size_bytes)
      self.new(size_bytes)
    end
    def self.alloc_out(size_bytes)
      self.new(size_bytes)
    end
    def self.alloc_inout(size_bytes)
      self.new(size_bytes)
    end

    def self.alloc_in(type, count)
      self.new(type, count)
    end
    def self.alloc_out(type, count)
      self.new(type, count)
    end
    def self.alloc_inout(type, count)
      self.new(type, count)
    end

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

    def put_bytes(offset, string, src_offset, src_size)
      # this variant gets bridge methods
	   # obj, one-based start offset, one-based end offset, zero-based 
      self.copyfrom_from_to_into(string, src_offset + 1, 
			src_offset + src_size, offset)
    end
    def put_bytes(offset, string)
      self.copyfrom_from_to_into(string, 1, string.length, offset)
			 	# obj, one-based, one-base, zero-based
    end
    def put_bytes(offset, string, src_offset)
      self.copyfrom_from_to_into(string, src_offset + 1,  string.length, offset)
			 	# obj, one-based, one-base, zero-based
    end

    def get_bytes(offset, length)
      self.stringfrom_to(offset, length-1) # both args zero based
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

    # def put_pointer(byteoffset, pointer) ; end
    #  pointer is a kind of CByteArray or a CPointer, or nil
    primitive_nobridge 'put_pointer', 'pointerAt:put:'

    def get_pointer(ofs)
      self.__struct_pointer_at(ofs)
    end

    def total
      self.size
    end

  end

end
