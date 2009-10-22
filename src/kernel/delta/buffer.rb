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

    def put_char(offset, val)
      self.int8_put(offset, val)
    end
    def put_uchar(offset, val)
      self.int8_put(offset, val)
    end
    def put_short(offset, val)
      self.int16_put(offset, val)
    end
    def put_ushort(offset, val)
      self.int16_put(offset, val)
    end
    def put_int(offset, val)
      self.int32_put(offset, val)
    end
    def put_uint(offset, val)
      self.int32_put(offset, val)
    end
    def put_long(offset, val)
      self.int64_put(offset, val)
    end
    def put_ulong(offset, val)
      self.int64_put(offset, val)
    end
    def put_long_long(offset, val)
      self.int64_put(offset, val)
    end
    def put_ulong_long(offset, val)
      self.int64_put(offset, val)
    end
    def put_bytes(offset, string)
      self.copyfrom_from_to_into(string, 1, string.length, offset)
			 	# obj, one-based, one-base, zero-based
    end
    def put_bytes(offset, string, src_offset)
      self.copyfrom_from_to_into(string, src_offset + 1,  string.length, offset)
			 	# obj, one-based, one-base, zero-based
    end
    def put_bytes(offset, string, src_offset, src_size)
      self.copyfrom_from_to_into(string, src_offset + 1, src_size, offset)
			 	# obj, one-based, one-base, zero-based
    end

    def get_bytes(offset, length)
      self.stringfrom_to(offset, length-1) # both args zero based
    end

    def get_string(offset, length)
      self.stringfrom_to(offset, length-1) # both args zero based
    end

    def get_string(offset)
       lim = self._search_for_zerobyte(offset) 
       if lim < offset
         lim = self.size
       end
       self.stringfrom_to(offset, lim - 1)  # both args zero based
    end

    def put_string(offset, string)
      len = string.length
      self.copyfrom_from_to_into(string, 1, len, offset)
			 	# obj, one-based, one-base, zero-based
      self.int8_put(offset + len, 0)  # add a null byte
    end

    def put_pointer(ofs, memory_pointer)
      pval = memory_pointer.read_pointer()
      self.put_long(ofs, pval);
    end

    def get_pointer(ofs)
      p = MemoryPointer.new
      p.write_pointer( self.get_long(ofs))
      p
    end

    def total
      self.size
    end

  end

end
