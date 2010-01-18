module FFI

  class StructLayout # [

    # def initialize ;end  # in ffi.rb for fixed instvars

    def __get(fieldname, cbytearray)
      idx = @members_dict[fieldname]
      if idx._equal?(nil)
        raise  ArgumentError, 'invalid field name'
      end
      ofs = @offsets[idx]
      elem_siz = @elem_sizes[idx]
      if elem_siz._not_equal?(0)
        if elem_siz._isFixnum   # a nested array
          siz = @sizes[idx]
          mp = MemoryPointer.__fromRegionOf(cbytearray, ofs, siz)
          mp.__initialize(elem_siz)
          mp
        else  # a nested Struct , elem_siz._kind_of?(Struct.class) == true
          unless elem_siz._kind_of?(Struct.class)  # do not checkin
             raise TypeError, 'logic error, expected a Struct class'
          end
          nested_struct_cls = elem_siz
          struct = nested_struct_cls.__fromRegionOf(cbytearray, ofs,  elem_siz.size)
          struct.__set_layout( nested_struct_cls.__cl_layout )
          struct.__initialize
        end
      else
        sel = @accessors[idx]
        cbytearray.__perform_se(ofs, sel, 1)  # assumes envId 1
      end
    end

    def __put(fieldname, cbytearray, value)
      idx = @members_dict[fieldname]
      if idx._equal?(nil)
        raise  ArgumentError, 'invalid field name'
      end
      ofs = @offsets[idx]
      elem_siz = @elem_sizes[idx]
      if elem_siz._not_equal?(0)
        raise ArgumentError, 'store to nested Array/Struct by value not supported'
        nil
      else
        sel = @setters[idx]
        cbytearray.__perform__se(ofs, value, sel, 1)  # assumes envId 1
      end
    end

    def size
      if @closed
        @totalsize
      else
        raise 'StructLayout build in progress'
      end
    end

    def __offsets
      @offsets
    end

    def offsets
      # Returns an array of name, offset pairs in layout order
      ofss = @offsets
      syms = @members
      lim = ofss.size
      arr = Array.new(lim)
      n = 0
      while n < lim
        arr[n] = [ syms[n], ofss[n] ]
        n += 1
      end
      arr
    end

    def offset_of(field_name)
      idx = @members_dict[field_name]
      @offsets[idx]
    end

    def members
      @members  # in layout order
    end

    def __add_member_name(name)
      if @members_dict[name]._equal?(nil)
        @members_dict[name] = @members.size
      else
        raise 'duplicate field name #{name}'
      end
    end

    def __check_offset(offset)
      if offset._equal?(nil)
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

    def __add_accessors( ctype )
      rubysel = StructAccessors[ctype]
      if rubysel._equal?(nil)
        raise 'internal error, inconsistent StructAccessors'
      end
      @accessors << rubysel
      rubysel = StructSetters[ctype]
      if rubysel._equal?(nil)
        raise 'internal error, inconsistent StructSetters'
      end
      @setters << rubysel
    end

    def __alignment
      @alignment
    end

    def add_field(name, type, offset)
      # Returns size in bytes of the added field
      if @closed
        raise 'cannot add more fields to a closed FFI::Struct'
      end
      unless name._isSymbol
        raise TypeError, 'field name must be a Symbol'
      end
      ctype = PrimTypeDefs[type]
      nstruct = nil
      if ctype._equal?(nil)
        if type._kind_of?(Struct.class)
          nstruct = type  # a nested Struct
          csize = type.size
          s_alignment = type.align
          unless (align = @totalsize % s_alignment) == 0
            @totalsize += s_alignment - align # add pad
          end
          if s_alignment > @alignment
            @alignment = s_alignment
          end
        else
          raise 'unrecognized field type ' , type.to_s
        end
      else
        csize = PrimTypeSizes[ctype]
        unless (align = @totalsize % csize) == 0
          @totalsize += csize - align # add pad
        end
        if csize > @alignment
          @alignment = csize
        end
      end
      offset = self.__check_offset(offset)
      self.__add_member_name(name)
      @members << name
      ofs = @totalsize
      @offsets << ofs
      if nstruct._equal?(nil)
        @elem_sizes << 0
        @sizes << csize
        self.__add_accessors( ctype )
      else
        @elem_sizes << nstruct  # a nested Struct
        @sizes << csize
        @accessors << nil
        @setters << nil
      end
      @totalsize += csize
      csize
    end

    def add_array(name, type, num_elements, offset)
      # Returns size in bytes of the added array
      if type._isSymbol
        ctype = PrimTypeDefs[type]
        if ctype._equal?(nil)
          raise 'unrecognized array element type ' , type.to_s
        end
        elemsiz = PrimTypeSizes[ctype]
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
      unless (align = @totalsize % elemsiz) == 0
        @totalsize += elemsiz - align # add pad
      end
      offset = self.__check_offset(offset)
      self.__add_member_name(name)
      @members << name
      @offsets << @totalsize
      @elem_sizes << elemsiz
      @sizes << csize
      @totalsize += csize
      @accessors << nil  # array access handled in __get ...
      @setters << nil
      csize
    end

    def close
      @closed = true
    end

  end # ]

  class UnionLayout # [

    def add_field(name, type, offset)
      save_size = @totalsize
      @totalsize = 0
      elem_siz = 0
      begin
        elem_siz = super(name, type, offset)
      ensure
        @totalsize = save_size
      end
      if elem_siz > save_size
        @totalsize = elem_siz
      end
      elem_siz
    end

    def add_array(name, type, num_elements, offset)
      save_size = @totalsize
      @totalsize = 0
      arr_size = 0
      begin
        arr_size = super(name, type, num_elements, offset)
      ensure
        @totalsize = save_size
      end
      if arr_size > save_size
        @totalsize = arr_size
      end
      arr_size
    end

  end # ]

  class Struct # [

    def self.new(*args)
      if args.size._equal?(1)
        ly = @cl_layout
        if ly._not_equal?(nil)
          arg = args[0]
          if arg._kind_of?(CByteArray)
            ly_siz = ly.size
            ba_siz = arg.total
            if ba_siz < ly_siz
              raise ArgumentError, "argument CByteArray is too small for a Struct  #{self.name}"
            end
            inst = self.__fromRegionOf( arg, 0, ly_siz )
          elsif arg._kind_of?(CPointer)
            inst = self.__fromCPointer( arg , ly.size )
          else
            raise TypeError, "cannot construct a Struct from a #{arg.class.name}"
            inst = nil
          end
          inst.__set_layout(ly)
          inst.initialize
          return inst
        end
      end
      ly = self.layout(*args)
      inst = self.__gc_malloc(ly.size)
      inst.__set_layout(ly)
      inst.initialize
      inst
    end

    def self.__layout_class
      StructLayout
    end

    def self.in
      :buffer_in
    end

    def self.out
      :buffer_out
    end

    def self.size
      @cl_layout.size
    end

    def self.members
      @cl_layout.members
    end

    def self.align
      @cl_layout.__alignment
    end

    def self.offsets
      @cl_layout.offsets
    end

    def self.offset_of(field_name)
      @cl_layout.offset_of(field_name)
    end

    def size
      @layout.size
    end

    def align
      @layout.__alignment
    end

    def members
      @layout.members
    end

    def values
      @layout.members.map { |m| self[m] }
    end

    def offsets
      @layout.offsets
    end

    def offset_of(field_name)
      @layout.offset_of(field_name)
    end

    # def clear ; end # inherited

#   def self.in  # Maglev TODO
#     :buffer_in
#   end

#   def self.out  # Maglev TODO
#     :buffer_out
#   end

    def [](field_name)
      @layout.__get(field_name, self)
    end

    def []=(field_name, value)
      @layout.__put(field_name, self, value)
    end

    def pointer
      # result is usable as arguments to an FFI function call
      self
    end

    def __initialize
      self.initialize
      self
    end

    protected # --------------------------------

#   def self.callback(params, ret)
#     mod = enclosing_module
#     FFI::CallbackInfo.new(find_type(ret, mod), params.map { |e| find_type(e, mod) })
#   end

    private # --------------------------------

    def self.enclosing_module
      begin
        mod = self.name.split("::")[0..-2].inject(Object) { |obj, c| obj.const_get(c) }
        mod.respond_to?(:find_type) ? mod : nil
      rescue Exception => ex
        nil
      end
    end

    def self.is_a_struct?(type)
      type._is_a?(Class) and type < Struct
    end

    def self.find_type(type, mod = nil)
      return type if is_a_struct?(type) or type._is_a?(::Array)
      mod ? mod.find_type(type) : FFI.find_type(type)
    end

    def self.hash_layout(*spec)
      raise "FFI::Struct hash_layout not supported by Maglev, must use array_layout"
    end

    def self.array_layout(*spec)
      builder = self.__layout_class.new
      mod = enclosing_module
      i = 0
      while i < spec.size
        name = spec[i]
        type = spec[i + 1]
        if type._equal?(nil)
          raise ArgumentError, "odd sized layout spec, type nil for #{name}"
        end
        i += 2
        # If the next param is a Fixnum, it specifies the offset
        offset = spec[i]
        if offset._isFixnum
          i += 1
        else
          offset = nil
        end
        if type._kind_of?(Class) && type < Struct
          builder.add_field(name, type, offset)
        elsif type._kind_of?(::Array)
          builder.add_array(name, find_type(type[0], mod), type[1], offset)
        else
          builder.add_field(name, find_type(type, mod), offset)
        end
      end
      builder.close
      builder
    end


    def self.layout(*spec)
      sp_size = spec.size
      if sp_size._equal?(0)
        return @cl_layout
      end
      if sp_size._equal?(1)
         if spec[0]._isHash
            raise "FFI::Struct hash_layout not supported by Maglev, must use array_layout"
         end
         raise ArgumentError , 'minimum argument size is 2'
      end
      cspec = spec[0]._isHash ? hash_layout(*spec) : array_layout(*spec)
      unless self._equal?(Struct)
        @cl_layout = cspec
      end
      return cspec
    end

    def self.__cl_layout
      @cl_layout
    end

    def self.alloc_in
      self.new
    end

    def self.alloc_out
      self.new
    end

    # def initialize ; end # in ffi.rb to get fixed instvars

  end  # ]


  class Union # [

    def self.__layout_class
      UnionLayout
    end

  end # ]
end
