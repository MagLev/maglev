module FFI

  class StructLayout # [

    # def initialize ;end  # in ffi.rb for fixed instvars

    def _get(fieldname, cbytearray)
      idx = @members_dict[fieldname]
      if idx.equal?(nil)
        raise  ArgumentError, 'invalid field name'
      end
      ofs = @offsets[idx]
      sel = @accessors[idx]
      cbytearray.__perform_se(ofs, sel, 1)  # assumes envId 1
    end

    def _put(fieldname, cbytearray, value)
      idx = @members_dict[fieldname]
      if idx.equal?(nil)
        raise  ArgumentError, 'invalid field name'
      end
      ofs = @offsets[idx]
      sel = @setters[idx]
      cbytearray.__perform__se(ofs, value, sel, 1)  # assumes envId 1
    end

    def size
      if @closed
        @totalsize
      else
        raise 'StructLayout build in progress'
      end
    end

    def offsets
      @offsets
    end

    def offset_of(field_name)
      idx = @members_dict[field_name]
      @offsets[idx]
    end

    def members
      @members  # in layout order
    end

    def _add_member_name(name)
      if @members_dict[name].equal?(nil)
        @members_dict[name] = @members.size
      else
        raise 'duplicate field name #{name}'
      end
    end

    def _check_offset(offset)
      if offset.equal?(nil)
        offset = @totalsize
      else
        unless offset._isFixnum && offset >= 0
          raise TypeError, 'field offset must be a Fixnum >= 0'
        end
        unless offset == @totalsize
          raise "specified offset #{offset} inconsistent with size+padding #{@totalsize} of previous fields"
        end
      end
      offset
    end
    
    def _add_accessors( ctype )
      rubysel = StructAccessors[ctype]
      if rubysel.equal?(nil)
        raise 'internal error, inconsistent StructAccessors'
      end
      @accessors << rubysel  
      rubysel = StructSetters[ctype]
      if rubysel.equal?(nil)
        raise 'internal error, inconsistent StructSetters'
      end
      @setters << rubysel  
    end


    def add_field(name, type, offset)
      if @closed
        raise 'cannot add more fields to a closed FFI::Struct'
      end
      unless name._isSymbol
        raise TypeError, 'field name must be a Symbol'
      end
      offset = self._check_offset(offset)
      ctype = TypeDefs[type]
      if ctype.equal?(nil)
        raise 'unrecognized field type ' , type.to_s 
      end
      csize = TypeSizes[ctype] 
      unless (align = @totalsize % csize) == 0 
        @totalsize += csize - align # add pad
      end 
      self._add_member_name(name)
      @members << name
      ofs = @totalsize
      @offsets << ofs
      @sizes << csize
      self._add_accessors( ctype )
      @totalsize += csize
    end

    def add_array(name, type, num_elements, offset)
      if type._isSymbol
        ctype = TypeDefs[type]
        if ctype.equal?(nil)
          raise 'unrecognized array element type ' , type.to_s
        end
        elemsiz = TypeSizes[ctype]
      elsif is_a_struct?(type)
        elemsiz = type.size
      else
        raise "invalid Type of array elements , #{type}"
      end
      unless num_elements._isFixnum && num_elements > 0
        raise "num_elements must be a Fixnum > 0"
      end
      csize = elemsiz * num_elements
      if csize > 100000000000
        raise "total Array size must be <= 100.0e9 bytes" 
      end
      offset = self._check_offset(offset)
      unless (align = @totalsize % elemsiz) == 0
        @totalsize += elemsiz - align # add pad
      end
      self._add_member_name(name)
      @members << name
      @offsets << @totalsize
      @sizes << csize
      self._add_array_accessors(elemsiz, num_elements)
      @totalsize += csize
    end

    def add_struct(name, type, offset)
       # TODO
       raise 'not implem'
    end

    def close
      @closed = true
    end

  end # ]

  class Struct # [

#   def self.by_value  # Maglev TODO
#     ::FFI::StructByValue.new(self)
#   end

    def self.size
      @layout.size
    end

    def self.members
      @layout.members
    end

#   def self.align
#     @layout.alignment
#   end

    def self.offsets
      @layout.offsets
    end

    def self.offset_of(field_name)
      @layout.offset_of(field_name)
    end

    def size
      self.class.size
    end

    def align
      self.class.align
    end

    def members
      layout.members
    end

    def values
      layout.members.map { |m| self[m] }
    end
    def offsets
      self.class.offsets
    end

    def offset_of(field_name)
      self.class.offset_of(field_name)
    end

    def clear
      @bytearray.clear
      self
    end

    def to_ptr
      @bytearray
    end

#   def self.in  # Maglev TODO
#     :buffer_in
#   end

#   def self.out  # Maglev TODO
#     :buffer_out
#   end

    protected

#   def self.callback(params, ret)
#     mod = enclosing_module
#     FFI::CallbackInfo.new(find_type(ret, mod), params.map { |e| find_type(e, mod) })
#   end

    private

#   def self.builder
#     StructLayoutBuilder.new
#   end

    def self.enclosing_module
      begin
        mod = self.name.split("::")[0..-2].inject(Object) { |obj, c| obj.const_get(c) }
        mod.respond_to?(:find_type) ? mod : nil
      rescue Exception => ex
        nil
      end
    end

    def self.is_a_struct?(type)
      type.is_a?(Class) and type < Struct
    end

    def self.find_type(type, mod = nil)
      return type if is_a_struct?(type) or type.is_a?(::Array)
      mod ? mod.find_type(type) : FFI.find_type(type)
    end

    def self.hash_layout(spec)
      raise "FFI::Struct hash_layout not supported by Maglev, must use array_layout" 
    end

    def self.array_layout(spec)
      builder = StructLayout.new
      mod = enclosing_module
      i = 0
      while i < spec.size
        name, type = spec[i, 2]
        i += 2
        
        # If the next param is a Fixnum, it specifies the offset
        if spec[i].kind_of?(Fixnum)
          offset = spec[i]
          i += 1
        else
          offset = nil
        end
        if type.kind_of?(Class) && type < Struct
          builder.add_struct(name, type, offset)
        elsif type.kind_of?(::Array)
          builder.add_array(name, find_type(type[0], mod), type[1], offset)
        else
          builder.add_field(name, find_type(type, mod), offset)
        end 
      end
      builder.close
      builder
    end


    public
    def self.layout(*spec)
      return @layout if spec.size == 0
      cspec = spec[0].kind_of?(Hash) ? hash_layout(spec) : array_layout(spec)
      @layout = cspec #  unless self == FFI::Struct
      @size = cspec.size
      return cspec
    end

    def self._layout
      @layout
    end

    # following are Maglev additions
    # def initialize ; end # in ffi.rb to get fixed instvars 

    def [](field_name)
      @layout._get(field_name, @bytearray)
    end

    def []=(field_name, value)
      @layout._put(field_name, @bytearray, value)
    end

    def pointer
      # result is usable as arguments to an FFI function call
      @bytearray
    end

  end
end
