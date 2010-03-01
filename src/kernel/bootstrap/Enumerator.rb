module Enumerable
  Fixnum__MAX = Fixnum::MAX
  
  class Enumerator
    # The current implementation does not use fibers or continuations

    def initialize(object, enum_sel = :each, *args)
      @ofs = 0
      @obj = object
      unless enum_sel._isSymbol
        raise TypeError, 'second arg must be a Symbol'
      end
      @enum_selector = enum_sel
      @extra_args = args
    end

    def initialize(&block)
      # per specs, requires 1.8.8 or above
      raise ArgumentError, 'Enumerator from block not supported in 1.8.7'
    end

    def each(&block)
      @obj.__send__( @enum_selector, *@extra_args, &block)   
    end

    def each_with_index(&block)
      n = 0
      @obj.__send__( @enum_selector, *@extra_args) { | elem | 
        block.call( elem , n )
        n += 1
      }
    end

    def each_with_object(memo, &block)
      @obj.__send__( @enum_selector, *@extra_args) { | elem | 
        block.call( elem , memo )
      }
    end
  
    # def next ; end # subclass responsibility

    def rewind
      @ofs = 0
    end

    alias with_index  each_with_index

    alias with_object each_with_object
  end

  # ArrayEnumerator classes are also used for String

  class ArrayEnumerator < Enumerator
    def next
      # return an element,  @obj[ofs]
      arr = @obj
      ofs = @ofs
      if ofs >= arr.__size
        raise StopIteration
      end
      @ofs = ofs + 1
      arr.__at(ofs)
    end
  end

  class FirstEnumerator < Enumerator
    def next
      if ofs > 0
        raise StopIteration
      end
      @ofs = ofs + 1
      @obj.each { |o| return o }
      raise StopIteration
    end
  end

  class ArrayWithIndexEnumerator < Enumerator
    def next
      # return an Array [ @obj[ofs], ofs ]
      arr = @obj
      ofs = @ofs
      if ofs >= arr.__size
        raise StopIteration
      end
      @ofs = ofs + 1
      [ arr.__at(ofs) , ofs ]
    end
  end

  class ArrayIndexEnumerator < Enumerator
    def next
      # return the current offset
      arr = @obj
      ofs = @ofs
      if ofs >= arr.__size
        raise StopIteration
      end
      @ofs = ofs + 1
      ofs
    end
  end

  class ArraySliceEnumerator < Enumerator 
    def initialize(slice_size, array, enum_sel = :each, *args )
      super(array, enum_sel, *args)
      cnt = Type.coerce_to(slice_size, Fixnum , :to_int)
      if cnt <= 0
        raise ArgumentError, 'slice size must be > 0'
      end
      @count = cnt
    end

    def next
      # return  @obj[ofs, count]
      arr = @obj
      ofs = @ofs
      if ofs >= arr.__size
        raise StopIteration
      end
      cnt = @count
      res = arr.__at(ofs, cnt)
      @ofs = ofs + cnt
      res
    end
  end

  class ArrayConsEnumerator < ArraySliceEnumerator 
    def next
      # return  @obj[ofs, count]
      arr = @obj
      ofs = @ofs
      cnt = @count
      if (ofs + cnt > arr.__size)
        raise StopIteration
      end
      res = arr.__at(ofs, cnt)
      @ofs = ofs + 1
      res
    end
  end


  class ArrayReverseEnumerator < Enumerator
    def initialize(array, enum_sel = :each, *args )
      super(array, enum_sel, *args)
      @ofs = array.__size - 1
    end 

    def next
      # return the element  @obj[ofs]
      arr = @obj
      ofs = @ofs
      if ofs < 0
        raise StopIteration
      end
      @ofs = ofs - 1
      arr.__at(ofs)
    end

    def rewind
      @ofs = @obj.__size - 1
    end
  end

  # ---------------------------------------------
  # complex Array enumerators reimplementing more than just the method   next

  class ArrayCombinationEnumerator < ArrayEnumerator
    def initialize(count, array, enum_sel, *args )
      super(array, enum_sel, *args)
      @count = count
      @values = nil
    end

    def __compute_values
      varr = []
      @obj.__send__( @enum_sel, @count ) { |e| varr << e }
      @ofs = 0
      @values = varr
      return varr
    end

    def next 
      varr = @values
      if varr._equal?(nil)
        varr = self.__compute_values 
      end
      ofs = @ofs
      if ofs >= varr.__size
        raise StopIteration
      end
      res = varr[ofs]
      @ofs = ofs + 1
    end

    def rewind
      @ofs = 0
      @values = nil # force recompute
    end

    def each(&block)
      @obj.__send__( @enum_sel, @count, &block)
    end

    def each_with_object(memo, &block)
      @obj.__send__( @enum_sel, @count) { | elem |
        block.call( elem, memo )
      }
    end

    def each_with_index(&block)
      n = 0
      @obj.__send__( @enum_sel, @count) { | elem | 
        block.call( elem , n )
        n += 1
      }
    end
  end 

  class ArrayCycleEnumerator < Enumerator
    def initialize(array, repeat_count)
      cnt = Type.coerce_to(repeat_count, Fixnum , :to_int)
      super(array, :cycle)
      @cycle_count = cnt
      @num_cycles = cnt
    end

    def next
      # return the element  @obj[ofs]
      arr = @obj
      ofs = @ofs
      if ofs >= arr.__size
        cnt = @cycle_count - 1
        @cycle_count = cnt
        if cnt <= 0
          raise StopIteration
        end
        ofs = -1  # restart at front of array
      end
      @ofs = ofs + 1
      arr.__at(ofs)
    end

    def rewind
      @ofs = 0
      @cycle_count = @num_cycles
    end

    def each(&block)
      @obj.cycle(@num_cycles, &block)
    end

    def each_with_object(memo, &block)
      @obj.cycle(@num_cycles) { | elem |
        block.call( elem, memo )
      }
    end

    def each_with_index(&block)
      lim = @num_cycles
      cnt = 0
      arr = @obj
      sz = arr.__size
      while cnt < lim
        n = 0
        enum_res = arr.each { |elem|
          block.call(elem, n)
          n += 1
        }
        if n < sz
          return enum_res # break happened in block
        end 
        cnt += 1
      end
      nil
    end

    alias with_index  each_with_index
    
    alias with_object each_with_object
  end

  class ArraySelectEnumerator < ArrayEnumerator
    def initialize(obj, enum_sel, *args )
      super(obj, enum_sel, *args)
      @values = nil
    end

    def __compute_values
      varr = []
      @obj.each { |e| varr << e }
      @ofs = 0
      @values = varr
      return varr
    end

    def next 
      varr = @values
      if varr._equal?(nil)
        varr = self.__compute_values 
      end
      ofs = @ofs
      if ofs >= varr.__size
        raise StopIteration
      end
      res = varr[ofs]
      @ofs = ofs + 1
    end

    def rewind
      @ofs = 0
      @values = nil # force recompute
    end

    def each(&block)
      vals = self.__compute_values 
      vals.__send__( @enum_sel, &block)
    end

    def each_with_object(memo, &block)
      vals = self.__compute_values 
      vals.__send__( @enum_sel, @count) { | elem |
        block.call( elem, memo )
      }
    end

    def each_with_index(&block)
      n = 0
      vals = self.__compute_values 
      vals.__send__( @enum_sel, @count) { | elem | 
        block.call( elem , n )
        n += 1
      }
    end
  end 

  class DirEnumerator < Enumerator

    def next
      # return an element,  @obj[ofs]
      dir = @obj
      arr = dir.__entries
      ofs = @ofs
      if ofs >= arr.__size
        raise StopIteration
      end
      dir.check_closed
      @ofs = ofs + 1
      arr.__at(ofs)
    end
  end

  class HashEnumerator < Enumerator
    def initialize(obj, enum_sel, *args )
      super(obj, enum_sel, *args)
      @values = nil
    end

    def __compute_values
      varr = []
      @obj.each { |k,v| varr << [k,v] }
      @ofs = 0
      @values = varr
      return varr
    end

    def next 
      varr = @values
      if varr._equal?(nil)
        varr = self.__compute_values 
      end
      ofs = @ofs
      if ofs >= varr.__size
        raise StopIteration
      end
      res = varr[ofs]
      @ofs = ofs + 1
    end

    def rewind
      @ofs = 0
      @values = nil # force recompute
    end
  end

  class HashKeyEnumerator < HashEnumerator
    # used to enumerate either keys or values
    def __compute_values
      varr = []
      @obj.__send__(@enum_sel) { |k| varr << k }
      @ofs = 0
      @values = varr
      return varr
    end
  end

end
Enumerable.__freeze_constants

class Array
  # optimize accesses to enumerator classes 
  ArrayEnumerator = Enumerable::ArrayEnumerator
  ArrayWithIndexEnumerator = Enumerable::ArrayWithIndexEnumerator
  ArrayIndexEnumerator = Enumerable::ArrayIndexEnumerator
  ArraySliceEnumerator = Enumerable::ArraySliceEnumerator
  ArrayConsEnumerator = Enumerable::ArrayConsEnumerator
  ArrayReverseEnumerator = Enumerable::ArrayReverseEnumerator
  ArrayCycleEnumerator = Enumerable::ArrayCycleEnumerator
  ArrayCombinationEnumerator = Enumerable::ArrayCombinationEnumerator
end
Array.__freeze_constants 

class Dir
  DirEnumerator = Enumerable::DirEnumerator
end
Dir.__freeze_constants

class Hash
  HashEnumerator = Enumerable::HashEnumerator
end
Hash.__freeze_constants

