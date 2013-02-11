module Enumerable

  class ObjectEnumerator < Enumerator
    # Object enumerator used for lazy enumeration  , fix Trac 904
    def next(&block)
      # return an element of the minor-enumeration
      @sub_enum ||= @obj.__send__(@enum_selector, *@extra_args).each
      @sub_enum.next(&block)
    end
  end

  class ArrayEnumerator < Enumerator
    # ArrayEnumerator also used for String#each_byte
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

  class StringByteEnumerator < Enumerator
    def next
      # return an element,  @obj[ofs]
      str = @obj
      ofs = @ofs
      if ofs >= str.__size
        raise StopIteration
      end
      @ofs = ofs + 1
      str.__at(ofs)
    end
  end

  class StringCharEnumerator < Enumerator
    def next
      # return an element,  @obj[ofs]
      str = @obj
      ofs = @ofs
      if ofs >= str.__size
        raise StopIteration
      end
      @ofs = ofs + 1
      ch = str.__at(ofs)
      res = ' ' .
      res.__at_put(0, ch)
      res 
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

    def each_with_index(&block)
      unless block_given?
        return self
      end
      @obj.each_with_index(&block)
    end

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


  class ArrayReverseEnumerator < Enumerator
    def initialize(array, enum_sel)
      super(array, enum_sel)
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
      self
    end
  end

  # ---------------------------------------------
  # complex Array enumerators reimplementing more than just the method   next

  class ArrayCombinationEnumerator < ArrayEnumerator
    def initialize(count, array, enum_sel)
      super(array, enum_sel)
      @count = count
      @values = nil
    end

    def __compute_values
      varr = []
      cnt = @count
      if cnt._equal?(nil)
        cnt = @obj.__size
      end
      @obj.__send__( @enum_selector, cnt ) { |e| varr << e }
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
      self
    end

    def each(&block)
      cnt = @count
      if cnt._equal?(nil)
        cnt = @obj.__size
      end
      @obj.__send__( @enum_selector, cnt, &block)
    end

    def each_with_object(memo, &block)
      cnt = @count
      if cnt._equal?(nil)
        cnt = @obj.__size
      end
      @obj.__send__( @enum_selector, cnt) { | elem |
        block.call( elem, memo )
      }
    end

    def each_with_index(&block)
      cnt = @count
      if cnt._equal?(nil)
        cnt = @obj.__size
      end
      n = 0
      @obj.__send__( @enum_selector, cnt) { | elem | 
        block.call( elem , n )
        n += 1
      }
    end
  end 

  class ArrayCycleEnumerator < Enumerator
    def initialize(array, repeat_count)
      cnt = Maglev::Type.coerce_to(repeat_count, Fixnum , :to_int)
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
      self
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
    def initialize(obj, enum_sel)
      super(obj, enum_sel)
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
      self
    end

    def each(&block)
      vals = self.__compute_values 
      vals.__send__( @enum_selector, &block)
    end

    def each_with_object(memo, &block)
      vals = self.__compute_values 
      vals.__send__( @enum_selector, @count) { | elem |
        block.call( elem, memo )
      }
    end

    def each_with_index(&block)
      n = 0
      vals = self.__compute_values 
      vals.__send__( @enum_selector, @count) { | elem | 
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
    def initialize(obj, enum_sel)
      super(obj, enum_sel)
      @deque_pointer = obj.__head
    end

    def next 
      @deque_pointer = @deque_pointer.instance_variable_get :@next
      
      if @deque_pointer.equal?(@obj.__tail)
        raise StopIteration
      end
       
      return [@deque_pointer.instance_variable_get(:@key), @deque_pointer.instance_variable_get(:@value)]
    end

    def rewind
      @deque_pointer = @obj.__head
      self
    end
  end

  class HashKeyEnumerator < HashEnumerator
    # used to enumerate either keys or values
    def next
      if @enum_selector.eql?(:each_value)
        return super[1]
      elsif @enum_selector.eql?(:each_key)
        return super[0]
      end
    end
  end

  class NumericEnumerator < Enumerator
    def initialize(object, start, last, increm)
      @obj = object
      @ofs = start
      @enum_selector = :each
      @extra_args = []

      @start = start 
      @last = last
      @inc = increm
    end

    def rewind
      @ofs = @start
      self
    end

    def __next
      n = @ofs
      if n <= @last
        @ofs = n + @inc
        return n
      end
      nil
    end

    def next
      n = self.__next
      if n._equal?(nil)
        raise StopIteration
      end
      n
    end
    def each(&block)
      self.rewind
      [1].each { |ignore| # this each handles break from the argument block
        while true
          n = self.__next
          if n._equal?(nil)
            return @obj
          end
          block.call(n)
        end
      }
    end
    def each_with_index(&block)
      self.rewind
      [1].each { |ignore| # this each handles break from the argument block
        k = 0
        while true
          n = self.__next
          if n._equal?(nil)
            return @obj
          end
          block.call(n, k)
          k += 1
        end
      }
    end

    def each_with_object(memo, &block)
      @obj.__send__( @enum_selector, *@extra_args) { | elem | 
        block.call( elem , memo )
      }
      self.rewind
      [1].each { |ignore| # this each handles break from the argument block
        k = 0
        while true
          n = self.__next
          if n._equal?(nil)
            return @obj
          end
          block.call(n, memo)
          k += 1
        end
      }
    end
  
    alias with_index  each_with_index

    alias with_object each_with_object
  end

  class NumericDownEnumerator < NumericEnumerator
    def __next
      n = @ofs
      if n >= @last
        @ofs = n - @inc
        return n
      end
      nil
    end
  end

  class RangeEnumerator <  NumericEnumerator
    def initialize(object, enum_sel, increment)
      # not using super.initialize 
      strt = object.begin # aRange.@from
      lst = object.end  # aRange.@to  
      @enum_selector = enum_sel
      @obj = object
      @start = strt 
      @ofs = strt
      @last = lst
      @inc = increment
      @exclude_end = object.exclude_end?
      @extra_args = []
      unless @start._isNumeric 
        unless @exclude_end
          @last = @last.succ
        end
      end
    end

    def rewind
      @ofs = @start
      self
    end

    def __next
      current = @ofs
      if @start._isNumeric
	if @exclude_end
          if current >= @last ; return nil; end
	else
          if current > @last ; return nil; end
        end
	nxt = current + @inc
      else
        if current >= @last ; return nil; end
        nxt = current.succ
      end 
      @ofs = nxt
      return current
    end
  end

  class IoEnumerator < Enumerator
    def next 
      if (anio = @obj).eof?
        raise StopIteration
      end      
      sep = @extra_args[0]
      res = anio.__next_line(sep)
      @ofs += 1
      res
    end

    def rewind
      @ofs = 0
      @obj.rewind
      self
    end
  end

  class IoByteEnumerator < IoEnumerator
    def next
      if (anio = @obj).eof?
        raise StopIteration
      end
      res = anio.readchar
      @ofs += 1
      res
    end
  end

  class IoCharEnumerator < IoEnumerator
    def next
      if (anio = @obj).eof?
        raise StopIteration
      end
      ch = anio.readchar
      str = ' ' 
      str[0] = ch
      @ofs += 1
      str
    end
  end

  class StringEachEnumerator < Enumerator # [
    def initialize(obj, enum_sel, separator )
      super(obj, enum_sel)
      @last = 0 
      @sep = separator
      @idx = separator.size
      @extra_args = [ separator ]
    end

    def next # [
      obj = @obj
      my_size = obj.__size
      sep = @sep
      sep_size = sep.__size
      newline = sep_size._equal?(0) ?  ?\n  : sep.__at(sep_size - 1)

      last = @last 
      i = @idx
      if sep_size._equal?(0)
	while i < my_size
	  if obj.__at(i)._equal?( ?\n )
	    if obj.__at(i+=1)._not_equal?( ?\n )
	      i += 1
	      next
	    end
	    i += 1 while i < my_size && obj.__at(i)._equal?( ?\n )
	  end
	  if i > 0 && obj.__at(i-1)._equal?( newline ) 
	    line = obj.__at(last, i-last)
            @last = i
            @idx = i + 1
            return line
	  end
	  i += 1
	end
      else
	while i < my_size
	  if i > 0 && obj.__at(i-1)._equal?(newline) &&
	      (sep_size < 2 || obj.__at_equals( i - sep_siz + 1, sep))
	    line = obj.__at(last, i-last)
            @last = i
            @idx = i + 1
	    return line 
	  end
	  i += 1
	end
      end
      unless last._equal?(my_size)
	line = obj.__at(last, my_size-last+1)
        @last = my_size + 1
        @idx = my_size + 1
	return line
      end
      raise StopIteration
    end  # ]
  end # ]

  class StringGsubEnumerator < Enumerator # 
    def initialize(obj, enum_sel, regex)
      super(obj, enum_sel)
      @regex = obj.__get_pattern( regex , true)
    end

    def next 
      pos = @ofs
      str = @obj
      str_siz = str.__size
      if pos >= str_siz
        raise StopIteration
      end
      match = @regex.__search(str, pos, nil)
      unless match
        @ofs = str_siz + 1
        raise StopIteration
      end 
      nxt_pos = match.end(0)
      if match.begin(0) == nxt_pos
        nxt_pos += 1
      end
      @ofs = nxt_pos
      match.__at(0)  # the matched substring
    end  
  end # ]

end
Enumerable.__freeze_constants

class Array
  # optimize accesses to enumerator classes 
  ArrayCombinationEnumerator = Enumerable::ArrayCombinationEnumerator
  ArrayCycleEnumerator = Enumerable::ArrayCycleEnumerator
  ArrayEnumerator = Enumerable::ArrayEnumerator
  FirstEnumerator = Enumerable::FirstEnumerator
  ArrayIndexEnumerator = Enumerable::ArrayIndexEnumerator
  ArrayReverseEnumerator = Enumerable::ArrayReverseEnumerator
  ArrayWithIndexEnumerator = Enumerable::ArrayWithIndexEnumerator
end
Array.__freeze_constants 

class Dir
  DirEnumerator = Enumerable::DirEnumerator
end
Dir.__freeze_constants

class Hash
  HashEnumerator = Enumerable::HashEnumerator
  HashKeyEnumerator = Enumerable::HashKeyEnumerator
end
Hash.__freeze_constants

class Integer
  NumericEnumerator = Enumerable::NumericEnumerator
  NumericDownEnumerator = Enumerable::NumericDownEnumerator
end
Integer.__freeze_constants
class Numeric
  NumericEnumerator = Enumerable::NumericEnumerator
  NumericDownEnumerator = Enumerable::NumericDownEnumerator
end
Numeric.__freeze_constants
class Float
  NumericEnumerator = Enumerable::NumericEnumerator
  NumericDownEnumerator = Enumerable::NumericDownEnumerator
end
Float.__freeze_constants

class IO
  IoEnumerator = Enumerable::IoEnumerator
  IoByteEnumerator = Enumerable::IoByteEnumerator 
  IoCharEnumerator = Enumerable::IoCharEnumerator 
end
IO.__freeze_constants

class Range
  RangeEnumerator = Enumerable::RangeEnumerator
end
Range.__freeze_constants

class String
  StringByteEnumerator = Enumerable::StringByteEnumerator
  StringCharEnumerator = Enumerable::StringCharEnumerator
  StringEachEnumerator  = Enumerable::StringEachEnumerator
  ArrayEnumerator = Enumerable::ArrayEnumerator
  StringGsubEnumerator = Enumerable::StringGsubEnumerator 
end
String.__freeze_constants

