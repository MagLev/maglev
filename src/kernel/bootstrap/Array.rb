class Array

  # TODO consider a method prefix __nobr__ which during load prims
  #   would suppress generation of bridge methods for private
  #   methods that should not need bridge methods

  # used in RubyRescueBodyNode>>initSplatRescueWith:and:at:
  def __anySatisfyCaseLeaf( obj )
    # used in implementation of  while *list   within a   case
    n = 0
    lim = self.__size
    while n < lim
      el = self.__at(n)
      if el === obj 
        return true
      end
      n = n + 1
    end
    false
  end

  def __anySatisfyCaseTrue
    # used in implementation of  while *list   within a   case
    n = 0
    lim = self.__size
    while n < lim
      el = self.__at(n)
      if el
        return true
      end
      n = n + 1
    end
    false
  end

  # Used by both assoc and rassoc, since they differ only in the index of
  # the array to compare to the key
  def __assoc(key, idx)
    i = 0
    lim = self.__size
    while i < lim
      el = self.__at(i)
      if el._isArray && el[idx] == key
        return el
      end
      i += 1
    end
    nil
  end

  def __as_hash
    # called from generated code
    Hash.__from_array(self)
  end

  primitive_nobridge '__fill_resize', 'fillFrom:resizeTo:with:'
  primitive_nobridge '__insertall_at', 'insertAll:at:'

  def __add_arguments(arg)
    # called from generated code
    if arg._isArray
      if arg.__size._not_equal?(0)
        self.__insertall_at(arg, self.__size + 1)
      end
    else
      a = arg._splat_lasgn_value_coerce
      if a._isArray
        if a.__size._not_equal?(0)
          self.__insertall_at(a, self.__size + 1)
        end
      else
        self.__push( a )
      end
    end
    self
  end

  def __flatten_onto(output, level)
    # returns true if any recursion done into child arrays
    recursed = false
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added
      # self was already in the set
      raise ArgumentError, 'recursive element in flatten'
    end
    begin
      i = 0
      lim = self.__size
      while i < lim
        el = self.__at(i)
        if level > 0
          if el._isArray
            el.__flatten_onto(output, level - 1)
            recursed = true
          elsif el.respond_to?(:to_ary)
            el = el.to_ary
            if el._isArray
              el.__flatten_onto(output, level - 1)
              recursed = true
            elsif el._equal?(nil)
              #ok
            else
              raise TypeError, 'in flatten, to_ary did not return an Array'
            end
          else
            output.__push( el )
          end
        else
          output.__push( el )
        end
        i = i + 1
      end
    ensure
      ts.remove(self)
    end
    recursed
  end
  # end private helper methods

  # Array Class Methods
  class_primitive_nobridge '__alloc', '_rubyNew:initValue:'
  class_primitive_nobridge '__withall', '_rubyWithAll:'

  # Returns a new array with the given elements.
  def self.[](*elements)
    __withall(elements)
  end

  # implementations of new need to call initialize in case application has
  #   subclassed Array and reimplemented initialize .
  def self.new(*args)
    # this variant gets bridge methods
    len = args.__size
    if len <= 2
      if  len._equal?(2)
        a = self.new(args[0], args[1])
      elsif len._equal?(1)
        a = self.new(args[0])
      elsif len._equal?(0)
        a = self.new
      end
      a
    else
      # if a subclass does not reimplement initialize*,
      #  this path raises ArgumentError, 'too many args' 
      a = self.__alloc(0, nil)
      a.initialize(*args) 
      a
    end
  end

  def initialize(*args, &block)
    # If a Subclass#initialize calls super(*args), then we go through this
    # variant.  That means we also have to unpack the parameters for all
    # the possible variants here.  The initialize variants below will be
    # called directly, in the case of Array, or of a subclass calling super
    # with explicit args, e.g. super(x,y).

    len = args.__size
    if len._equal?(0)
      self
    elsif len._equal?(1)
      __initialize(args[0], &block)
    elsif len._equal?(2)
      __initialize(args[0], args[1])
    else
      raise ArgumentError, 'too many args'
    end
  end

  def self.new(first, &block)
    if first._isArray
      a = __withall(first) # ignore the block
    else
      siz = Maglev::Type.coerce_to(first, Fixnum, :to_int)
      a = __alloc(siz, nil)
      # block processed by initialize
    end
    a.initialize(first, &block)
    a
  end

  def initialize(first, &block)
    __initialize(first, &block)
  end

  def __initialize(first, &block)
    if self.class._equal?(Array)
      if first._isArray
        lim = 0 # ignore any block
      else
        lim = self.__size
      end
    else
      if first._isArray
        self.replace(first)
        lim = 0 # ignore any block
      else
        lim = Maglev::Type.coerce_to(first, Fixnum, :to_int)
        self.__size=(lim)
      end
    end
    if block_given?
      n = 0
      while (n < lim)
        self.__at_put(n,  block.call(n))
        n = n + 1
      end
    end
    self
  end


  def self.new(a_size, value)
    if self._equal?(Array)
      s = Maglev::Type.coerce_to(a_size, Fixnum, :to_int)
      a = __alloc(s, value)
    else
      a = __alloc(0, nil)
    end
    a.initialize(a_size, value)
    a
  end

  def initialize(a_size, value)
    if self.class._equal?(Array)
      # do nothing
    else
      s = Maglev::Type.coerce_to(a_size, Fixnum, :to_int)
      self.__size=(s)
      self.fill(value, 0, s)
    end
    self
  end
  def __initialize(a_size, value)
    if self.class._equal?(Array)
      # do nothing
    else
      s = Maglev::Type.coerce_to(a_size, Fixnum, :to_int)
      self.__size=(s)
      self.fill(value, 0, s)
    end
    self
  end

  def self.new(first, second, &block)
    if block_given?
      self.new(first, &block)  # ignore second arg
    else
      self.new(first, second)
    end
  end

  def self.new(arg)
    # this method will have no bridge methods, all the bridges
    #  will map to the previous 2 arg form
    if self._equal?(Array)
      if arg._isFixnum
        a = __alloc(arg, nil)
        return a
      elsif arg._isArray
        a = __alloc(arg.__size, nil)
      else
        arg = __coerce_one_arg(arg)
        if arg._isFixnum
          a = __alloc(arg, nil)
          return a
        else
          a = __alloc(arg.__size, nil)
        end
      end
    else
      a = __alloc(0, nil)
    end
    a.initialize(arg)
    a
  end

  def self.__coerce_one_arg(arg)
    carg = Maglev::Type.coerce_to_or_nil(arg, Array, :to_ary)
    if carg._equal?(nil)
      carg = Maglev::Type.coerce_to(arg, Fixnum, :to_int)
    end
    carg
  end

  def initialize(arg)
    if arg._isFixnum
      self.__size=(arg)
    elsif arg._isArray
      self.replace(arg)
    else
      arg = Array.__coerce_one_arg(arg)
      if arg._isFixnum
        self.__size=(arg)
      else
        self.replace(arg)
      end
    end
    self
  end

  def self.new
    a = __alloc(0, nil)
    a.initialize
    a
  end

  def initialize
    self.__size=(0)
    self
  end

  def self.new(&block)
    # ignores the block
    a = __alloc(0, nil)
    a.initialize(&block)
    a
  end

  def initialize(&block)
    self.__size=(0)
    self
  end

  primitive   '__basic_dup', '_rubyBasicDup'      # use non-singleton class

  def dup
    res = self.__basic_dup
    res.initialize_copy(self)
    res
  end

  # double quoted string literals support

  def __joinStrings
    # called from generated code
    str = ''
    n = 0
    siz = self.__size
    while n < siz
      str << self.__at(n).to_s
      n += 1
    end
    str
  end

  def __joinStringsAsSymbol
    # called from generated code
    # use intern to reject empty symbol and ?0 characters
    self.__joinStrings.intern
  end

  def __joinStringsWithRegexpOptions(opts_integer)
    # called from generated code
    # inline variant of __joinStrings
    str = ''
    n = 0
    siz = self.__size
    while n < siz
      str << self.__at(n).__regex_to_s
      n += 1
    end
    str
    # bypass Regexp.new because opts_integer includes both
    #  the options and the language encoding
    rx = Regexp.alloc
    res = rx.__compile(str, opts_integer)
    if res._not_equal?(rx)
      raise RegexpError, (res.to_str)  # error from onig_new
    end
    res
  end

  # Set intersection.
  # Return new array containing elements common to two arrays.
  def &(other)
    other = Maglev::Type.coerce_to(other, Array, :to_ary)
    my_siz = self.__size
    other_siz  = other.__size
    dflt = Object.new
    htsiz = (my_siz + other_siz).__divide(4)
    htsiz = 5 if htsiz < 5
    dict = Hash.__new(htsiz)
    n = 0
    while n < other_siz
      elem = other[n]
      dict[ elem ] = elem
      n += 1
    end
    res = self.class.__alloc( my_siz < other_siz ? my_siz : other_siz , nil )
    n = 0
    res_idx = 0
    while n < my_siz
      elem = self.__at(n)
      if dict.__delete_ifpresent(elem)
        res[res_idx] = elem
        res_idx += 1
      end
      n += 1
    end
    res.__size=(res_idx)
    res
  end

  # Repetition: if +obj+ is a string, then <tt>arr.join(str)</tt>, otherwise
  # return a new array by concatenating +obj+ copies of self.
  def *(obj)
    if obj.respond_to?( :to_str )
      return join(obj)
    end
    val = Maglev::Type.coerce_to(obj, Fixnum, :to_int)
    if val < 0
      raise ArgumentError, "arg must be >= 0"
    end
    result = self.class.__alloc(0, nil)
    i = 0
    while i < val
      result.concat(self)
      i += 1
    end
    result
  end

  def +(arg)
    arg = Maglev::Type.coerce_to(arg, Array, :to_ary)
    res = self.dup
    if arg.__size._not_equal?(0)
      res.__insertall_at(arg, res.__size + 1)
    end
    res
  end

  def -(arg)
    arg = Maglev::Type.coerce_to(arg, Array, :to_ary)
    argSize = arg.__size
    mySize = self.__size
    default = []
    h = Hash.new(default)
    res = []
    i = 0
    while i < argSize
      el = arg[i]
      h[el] = el
      i = i + 1
    end
    i = 0
    while i < mySize
      el = self.__at(i)
      if h[el]._equal?(default)
        res.__push( el )
      end
      i = i + 1
    end
    res
  end

  # note, <<  can't use smalltalk add: , it returns arg, not receiver
  primitive '<<', '_rubyAddLast:'

  # This "alias" is needed by marshal.rb.  We need an entry point that
  # won't be overridden by sub-classes.
  primitive_nobridge '__ruby_add_last', '_rubyAddLast:'

  # Comparison: Returns an integer -1, 0, or 1 if this array is less than,
  # equal to or greater than +other+.  Each object in each array is
  # compared using <tt><=></tt>.  If any value isn't equal, then that
  # inequality is the return value.  If all the values found are eqal, then
  # the return is based on a comparison of the array lengths.  Thus, two
  # arrays are "equal" iff they have the same length and the value of each
  # element is equal to the value oof the corresponding element in the
  # other array.
  #
  # If other is not an array, raise a type error "TypeError
  def <=>(other)
    other = Maglev::Type.coerce_to(other, Array, :to_ary)
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    begin
      other_size = other.__size
      my_size = self.__size
      lim = my_size > other_size ? other_size : my_size # lim is the min
      i = 0
      while i < lim
        curr = self.__at(i)
        if ts.include?(curr)
          unless curr._equal?(other[i])
            return 1 if my_size > other_size
            return -1 if my_size < other_size
          end
        else
          result =  curr <=> other[i]
          return result if result._not_equal?(0)
        end
        i += 1
      end
      return my_size <=> other_size
    ensure
      if added
        ts.remove(self)
      end
    end
  end

 
  # ====== Comparable:
  # RxINC: This is a cut-n-paste to get things working for mspec.
  # Need to either overwrite or allow a mixin.

  # Return true if both are the same object, or if both are arrays, and
  # have the same number of elements and all corresponding elements are
  # eql?.
  #

  def eql?(other)
    return true if self._equal?(other)
    return false unless other._isArray
    lim = self.__size
    unless lim._equal?(other.__size)
      return false
    end
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    begin
      i = 0
      limi = lim
      while i < limi
        v = self.__at(i)
        ov = other[i]
        if v._equal?(ov)
          # ok
        elsif ts.include?(v) || ts.include?(ov)
          # v and ov are the same objects, so behave like MRI 1.8.7 and return false
          # in MRI 1.9.3 it should return true, though the array are not the same objects, but they have the same content
          return true          
        elsif v.eql?(ov)
          # ok
        else
          return false
        end
        i += 1
      end
    ensure
      if added
        ts.remove(self)
      end
    end
    true
  end

  alias :== :eql?

  def >(other)
    (self <=> other) > 0
  end

  def <(other)
    (self <=> other) < 0
  end

  def >=(other)
    (self <=> other) >= 0
  end

  def <=(other)
    (self <=> other) <= 0
  end

  def between?(min, max)
    (min <= self) && (self <= max)
  end
  # ==== end Comparable

  primitive_nobridge '[]' , '_rubyAt:'
  primitive_nobridge '[]' , '_rubyAt:length:'

  # variants for use within bootstrap code, after the first use of second arg,
  #  so first use will show up in profiling results.
  primitive_nobridge '__at' , '_rubyAt:'
  primitive_nobridge '__at' , '_rubyAt:length:'

  primitive_nobridge '__at_put', '_rubyAt:put:'
  primitive_nobridge '__at_put', '_rubyAt:length:put:'


  def [](*args)
    len = args.__size
    if len._equal?(1)
      slice(args[0])
    elsif len._equal?(2)
      slice(args[0], args[1])
    else
      raise ArgumentError, 'expected 1 or 2 args'
    end
  end

  def []=(*args)
    na = args.__size
    if na._equal?(2)
      self[args.__at(0)] = args.__at(1)
    elsif na._equal?(3)
      self[args.__at(0), args.__at(1)] = args.__at(2)
    else
      raise ArgumentError, 'expected 2 or 3 args'
    end
  end

  primitive_nobridge '[]=', '_rubyAt:length:put:'
  primitive_nobridge '[]=', '_rubyAt:put:'

  # Set Union.  Removes duplicates from self: [1,1,1] | [] => [1]
  def |(other)
    other = Maglev::Type.coerce_to(other, Array, :to_ary)
    hash = {}
    ary = []
    i = 0
    lim = self.__size
    while i < lim
      el = self.__at(i)
      unless hash.include? el
        ary.__push( el )
        hash[el] = el
      end
      i = i + 1
    end

    i = 0
    lim = other.__size
    while i < lim
      el = other[i]
      unless hash.include? el
        ary.__push( el )
        hash[el] = el
      end
      i = i + 1
    end
    ary
  end

  # Associate: Search through self (an array of arrays).  Return first
  # array whose first element matches +key+ (using +key.==+).
  def assoc(key)
    __assoc(key, 0)
  end

  primitive 'at' , '_rubyAt:'

  # def choice ; end # added in 1.8.7 , removed in 1.9 , not implemented in Maglev

  primitive 'clear', 'removeAll'

  # def clone ; end # inherited from Object

  def collect!(&block)
    unless block_given?
      return ArrayEnumerator.new(self, :collect! ) # for 1.8.7
    end
    i = 0
    sz = self.__size
    enum_res = self.each { | elem |  # use self.each to handle break in block
      self.__at_put(i, block.call( elem ) )
      i += 1
    }
    if i < sz
      return enum_res
    end
    self
  end

  primitive_nobridge '__copy_from_to', 'copyFrom:to:'

  primitive_nobridge '__basic_dup_named_ivs', '_rubyBasicDupNamedIvs' 
    # uses non-singleton class and copies fixed and dynamic instVars
    # but not the varying instVars accessed by []

  # Return copy of self with all nil elements removed
  def compact
    result = self.__basic_dup_named_ivs
    i = 0
    lim = self.__size
    while i < lim
      el = self.__at(i)
      result.__push( el ) unless el._equal?(nil)
      i += 1
    end
    result
  end

  # Remove all nil elements from self.  Return nil if no changes,
  # otherwise return self.
  def compact!
    i = 0
    lim = self.__size
    while i < lim
      break if self.__at(i)._equal?(nil)
      i += 1
    end
    return nil if i._equal?(lim)

    fill_idx = i
    while i < lim
      el = self.__at(i)
      unless el._equal?(nil)
        self.__at_put(fill_idx,  el )
        fill_idx += 1
      end
      i += 1
    end
    self.__size = fill_idx
    self
  end

  primitive_nobridge 'concat*', '_rubyAddAll:'

  def concat(arg)
    arg = Maglev::Type.coerce_to(arg, Array, :to_ary)
    if arg.__size._not_equal?(0)
      self.__insertall_at(arg, self.__size + 1)
    end
    self
  end

  def cycle(count=Fixnum__MAX, &block)
    if count._equal?(nil)
      count = Fixnum__MAX
    end
    unless block_given?
      return ArrayCycleEnumerator.new(self, count) # for 1.8.7
    end
    lim = Maglev::Type.coerce_to(count, Fixnum , :to_int)
    if lim <= 0
      return nil # do nothing
    end
    cnt = 0
    sz = self.__size
    if sz._equal?(0)
      return nil  # receiver is empty
    end
    while cnt < lim
      n = 0
      enum_res = self.each { |elem|
        block.call(elem)
        n += 1
      }
      if n < sz
        return enum_res # break happened in block
      end
      cnt += 1
    end
    nil
  end

  def delete(obj, &block)
    n = self.__size - 1
    found = false
    while n >= 0
      if self.__at(n) == obj
        oidx = n + 1
        self.__remove_from_to_(oidx, oidx)
        found = true
      end
      n = n - 1
    end
    if found._equal?(false)
      if block_given?
        block.call
      else
        nil
      end
    else
      obj
    end
  end

  def delete(obj)
    n = self.__size - 1
    res = nil
    while n >= 0
      if self.__at(n) == obj
        oidx = n + 1
        self.__remove_from_to_(oidx, oidx)
        res = obj
      end
      n = n - 1
    end
    return res
  end


  # Delete element at specified +index+.  Return the deleted item, or
  # +nil+ if no item at +index+
  def delete_at(idx)
    idx = Maglev::Type.coerce_to(idx, Fixnum, :to_int)
    sz = self.__size
    if idx < 0
      idx = sz + idx
      if idx < 0
        return nil
      end
    end
    if idx >= sz
      return nil
    end
    elem = self.__at(idx)
    oidx = idx + 1
    self.__remove_from_to_(oidx, oidx)
    return elem
  end

  # Delete every element of self for which +block+ evalutes to +true+.
  def delete_if(&block)
    unless block_given?
      return ArrayEnumerator.new(self, :delete_if) # for 1.8.7
    end
    i = 0
    lim = self.__size
    while i < lim
      break if block.call(self.__at(i) )
      i += 1
    end

    fill_idx = i
    while i < lim
      el = self.__at(i)
      unless block.call(el)
        self.__at_put(fill_idx,  el)
        fill_idx += 1
      end
      i += 1
    end
    self.__size=(fill_idx)
    self
  end

  def drop(count) # added in 1.8.7
    # return an Array containing self[count .. self.size -1]
    cnt = Maglev::Type.coerce_to(count, Fixnum, :to_int)
    if cnt < 0
      raise ArgumentError, 'arg to drop must be >= 0'
    end
    sz = self.__size
    if cnt >= sz
      return []
    end
    return self.__at(cnt, sz) # primitive truncates sz arg
  end

  def drop_while(&block)
    unless block_given?
      return ArrayEnumerator.new(self, :drop_while) # for 1.8.7
    end
    sz = self.__size
    broke = false
    n = 0
    all_true = true
    enum_res = self.each { |elem|
      broke = true
      blk_res =  block.call(elem)
      broke = false
      unless blk_res
        all_true = false
        break
      end
      n += 1
    }
    if broke
      return enum_res  # the argument block did a break
    end
    if all_true
      return []  # block always returned true
    end
    self.drop(n)
  end


  # def dup ; end # inherited from Object

  def each(&block)
    unless block_given?
      return ArrayEnumerator.new(self, :each ) # for 1.8.7
    end
    i = 0
    lim = self.__size
    while i < lim
      block.call(self.__at(i))
      i += 1
    end
    self
  end

  def each_with_index(&block)
    unless block_given?
      return ArrayWithIndexEnumerator.new(self, :each_with_index) # for 1.8.7
    end
    i = 0
    lim = self.__size
    while i < lim
      block.call(self.__at(i), i)
      i += 1
    end
    self
  end


  def each_index(&block)
    unless block_given?
      return ArrayIndexEnumerator.new(self, :each_index ) # for 1.8.7
    end
    i = 0
    lim = self.__size
    while i < lim
      block.call(i)
      i += 1
    end
    self
  end

  primitive 'empty?', 'isEmpty'

  def fetch(index, default=MaglevUndefined, &block)
    # variant with bridge methods
    if block_given?
      # per specs, block has precedence over default
      self.fetch(index, &block)  
    elsif default._equal?(MaglevUndefined) 
      self.fetch(index)
    else
      self.fetch(index, default)
    end
  end

  def fetch(index, &block)
    idx = Maglev::Type.coerce_to(index, Fixnum, :to_int)
    my_siz = self.__size
    bad = false
    if idx < 0
      idx = my_siz + idx
      if idx < 0
        bad = true
      end
    end
    if idx >= my_siz
      bad = true
    end
    if bad._equal?(true)
      if block_given?
        return block.call(index)
      else
        raise IndexError , 'offset out of bounds'
      end
    end
    self.__at(idx)
  end

  def fetch(index)
    idx = Maglev::Type.coerce_to(index, Fixnum, :to_int)
    my_siz = self.__size
    if idx < 0
      idx = my_siz + idx
      if idx < 0
        raise IndexError , 'Array#fetch, implied offset is negative'
      end
    end
    if idx >= my_siz
      raise IndexError , 'Array#fetch, offset beyond end'
    end
    self.__at(idx)
  end

  def fetch(index, default)
    idx = Maglev::Type.coerce_to(index, Fixnum, :to_int)
    my_siz = self.__size
    if idx < 0
      idx = my_siz + idx
      if idx < 0
        return default
      end
    end
    if idx >= my_siz
      return default
    end
    self.__at(idx)
  end

  def fill(a1=MaglevUndefined, a2=MaglevUndefined, a3=MaglevUndefined, &block)
    uu = MaglevUndefined
    if a3._equal?(uu)
      if a2._equal?(uu)
        if a1._equal?(uu)
          # zero args
          self.fill(nil, nil, &block)  # fails if no block given
        else
          # one arg
          self.fill(a1, &block) # fails if no block given
        end
      else
        # 2 args
        if block_given?
          self.fill(a1, a2, &block)
        else
          self.fill(a1, a2)
        end
      end
    else
      self.fill(a1, a2, a3)  # 3 args, block ignored
    end
  end

  def fill(&block)
    # no bridge methods for second and later variants
    fill(nil, nil, &block)
    self
  end

  def fill(start, &block)
    if start._isRange
      s = Maglev::Type.coerce_to(start.begin, Fixnum, :to_int)
      e = Maglev::Type.coerce_to(start.end,   Fixnum, :to_int)
      s += self.__size if s < 0
      e += self.__size if e < 0
      if start.exclude_end?
        return self if s == e
        e -= 1
      end
      raise RangeError, "#{start.inspect} out of range" if s < 0
      return self if e < 0
      s.upto(e) do | n |
        self[n] = block.call(n)
      end
    elsif block_given?
      self.fill(start, nil, &block) 
    else
      self.fill(start, nil, nil)
    end
    self
  end

  def fill(obj, start)
    # note no bridge methods for second and later variants
    if start._isRange
      s = Maglev::Type.coerce_to(start.begin, Fixnum, :to_int)
      e = Maglev::Type.coerce_to(start.end,   Fixnum, :to_int)
      s += self.__size if s < 0
      e += self.__size if e < 0
      if start.exclude_end?
        return self if s == e
        e -= 1
      end
      raise RangeError, "#{start.inspect} out of range" if s < 0
      return self if e < 0
      s.upto(e) do | n |
        self[n] = obj
      end
    else
      fill(obj, start, nil)
    end
    self
  end

  def fill(start, length, &block)
    unless start._isFixnum
      if start._equal?(nil)
        start = 0
      else
        start = Maglev::Type.coerce_to(start, Fixnum, :to_int)
      end
    end
    sz = self.__size
    if start < 0
      start = sz + start
      if start < 0
        start = 0
      end
    end
    if length._equal?(nil)
      length = sz - start
    else
      unless length._isFixnum
        length = length.to_int
      end
    end
    n = start
    limit = start + length
    unless block_given?
      raise ArgumentError, 'no block given'
    end
    while n < limit
      self.__at_put(n, block.call(n) )
      n = n + 1
    end
    self
  end

  def fill(obj, start, length)
    if start._equal?(nil)
      start = 0
    else
      start = Maglev::Type.coerce_to(start, Fixnum, :to_int)
    end
    sz = self.__size
    unless length._isFixnum
      unless length._equal?(nil)
        if length._kind_of?(Bignum)
          raise RangeError, "#{length} too big" if length >= 2**63
        else
          length = Maglev::Type.coerce_to(length, Fixnum, :to_int)
        end
        raise ArgumentError if length > sz
      end
    end
    if start < 0
      start = sz + start
      if start < 0
        start = 0
      end
    end
    if length._equal?(nil)
      if start >= sz
        # no modifications if index greater than end and no size
        return self
      end
      length = sz - start
    elsif length < 0
      length = 0
    end
    # smalltalk arrays start at 1
    endIdx = start + length

    start += 1         # start, end both 1-based now
    if endIdx > sz
      if length > 0
        __fill_resize(start, endIdx, obj) # resize and fill
      else
        self.__size=(endIdx)  # grow the receiver
      end
    elsif length > 0
      __fill_resize(start, 0 - endIdx, obj) # fill without resize
    end
    self
  end

  def first(*args, &block)
    sz = args.__size
    if sz._equal?(0)
      self.__at(0)
    elsif sz._equal?(1)
      self.first(args[0])
    else
      raise ArgumentError, 'Array#first, too many args'
    end
  end

  def first(count)
    cnt = Maglev::Type.coerce_to(count, Fixnum, :to_int)
    if cnt < 0
      raise ArgumentError, 'negative count'
    end
    self.__at(0, cnt)
  end

  def first
    self.__at(0)
  end

  def flatten(level = -1)  # level arg added for 1.8.7
    lev = Maglev::Type.coerce_to(level, Fixnum, :to_int)
    if lev._equal?(0)
      return self
    end
    ary = self.class.__alloc(0, nil)
    if lev < 0
      lev = Fixnum__MAX
    end
    __flatten_onto( ary , lev )
    ary
  end

  def flatten!(level = -1)  # level arg added for 1.8.7
    lev = Maglev::Type.coerce_to(level, Fixnum, :to_int)
    if lev._equal?(0)
      return nil
    end
    ary = []
    if lev < 0
      lev = Fixnum__MAX
    end
    recursed = __flatten_onto(ary, lev)
    if recursed
      self.replace(ary)
      self
    else
      nil
    end
  end

  def hash
    # sample at most 5 elements of receiver
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added
      return 0
    end
    hval = 4459
    begin
      mysize = self.__size
      interval = (mysize - 1).__divide(4)
      if interval < 1
        interval = 1
      end
      n = 0
      while n < mysize
        elem = self.__at(n)
        eh = elem.hash
        if eh._not_equal?(0)
          unless eh._isFixnum
            eh = eh & 0xfffffffffffffff # truncate to Fixnum
          end
          hval = (hval >> 1) ^ eh
        end
        n += interval
      end
    ensure
      ts.remove(self)
    end
    hval
  end

  # Note: The Pick Axe book has this method documented under both Array and
  # Enumerable.
  def include?(obj)
    n = 0
    lim = self.__size
    while n < lim
      if self.__at(n) == obj
        return true
      end
      n += 1
    end
    false
  end

  primitive '__includes_identical', 'includesIdentical:'
  primitive '__offset1_identical', 'indexOfIdentical:' # result is one-based

  def index(a1=MaglevUndefined, &block)  # added for 1.8.7
    if a1._equal?(MaglevUndefined)
      self.index(&block)
    else
      self.index(a1) # ignore block when first arg given
    end
  end

  def index(element)
    i = 0
    lim = self.__size
    while i < lim
      if self.__at(i) == element
        return i
      end
      i += 1
    end
    nil
  end

  def index(&block)  # added for 1.8.7
    unless block_given?
      return FirstEnumerator.new(self, :index )
    end
    i = 0
    lim = self.__size
    while i < lim
      if block.call(self.__at(i))
        return i
      end
      i += 1
    end
    nil
  end

  alias find_index index  # added for 1.8.7

  def indexes(*args)
    # use alias
    raise "Method not implemented: deprecated (use Array#values_at): Array#indexes"
  end
  def indicies(*args)
    # use alias
    raise "Method not implemented: deprecated (use Array#values_at): Array#indicies"
  end

  # If +index+ is not negative, inserts the given values before the element
  # with the given index.  If +index+ is negative, add the values after the
  # element with the given index (counting from the end).
  def insert(idx, *args)
    return self if args.__size == 0
    idx = Maglev::Type.coerce_to(idx, Fixnum, :to_int)
    idx += (size + 1) if idx < 0
    raise IndexError, "#{idx} out of bounds" if idx < 0
    self[idx, 0] = args
    self
  end

  def inspect
    s = "["
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added
      s << '...]'
      return s
    end
    begin
      s << ( collect {|ea| ea.inspect }.join(", ") )
      s << "]"
    ensure
      ts.remove(self)
    end
    s
  end

  def last(*args)               # fix Trac 903
    nargs = args.__size
    if nargs._equal?(0)
      self.last
    elsif nargs._equal?(1)
      self.last(args[0])
    else
      raise ArgumentError, 'expected 0 or 1 args'
    end
  end

  def last(count)
    cnt = Maglev::Type.coerce_to(count, Fixnum, :to_int)
    my_size = self.__size
    if cnt <= 0
      if cnt < 0
        raise ArgumentError, 'count must be >= 0'
      end
      return self.class.__alloc(0, nil)
    end
    ofs = my_size - cnt
    if ofs < 0
      ofs = 0
    end
    self.__at(ofs, cnt)
  end

  def last
    my_size = self.__size
    if my_size._equal?(0)
      return nil
    end
    return self.__at(my_size - 1)
  end

  primitive 'length', 'size'
  primitive_nobridge '__size', 'size'  # for use within bootstrap

  alias map! collect!

  # Number of non-<tt>nil</tt> elements in self.
  def nitems
    count = 0
    i = 0
    lim = self.__size
    while i < lim
      count += 1 unless self.__at(i)._equal?(nil)
      i += 1
    end
    count
  end

  primitive '__pack', 'rubyPack:'

  def pack(template_str)
    template_str = Maglev::Type.coerce_to(template_str, String, :to_str)
    self.__pack(template_str)
  end

  def self.__pack_coerce(obj, sym)
    # sent from C code in capiprim.c
    # sym includes the selector suffix
    begin
      r = obj.__perform__(sym, 1)
    rescue Exception
      return nil  # capiprim.c will raise TypeError
    end
    r
  end

  def pop(count=MaglevUndefined)  # added in 1.8.7
    if count._equal?(MaglevUndefined) 
      return self.pop()
    end 
    cnt = Maglev::Type.coerce_to(count, Fixnum, :to_int)
    if cnt < 0
      raise ArgumentError, 'arg to pop must be >= 0'
    end
    sz = self.__size
    if sz._equal?(0)
      return []
    end
    idx = sz - cnt
    if idx < 0
      res = self.dup
      self.__size=(0)
      return res
    end
    res = self.__at(idx, cnt)
    self.__size=(idx)
    res
  end

  def pop
    sz = self.__size
    elem = nil
    unless sz._equal?(0)
      idx = sz - 1
      elem = self.__at(idx)
      self.__size=(idx)
    end
    elem
  end

  primitive 'push*', '_rubyAddAll:'
  primitive_nobridge 'push', '_rubyAddLast:'
  primitive '__push*', '_rubyAddAll:'
  primitive_nobridge '__push', '_rubyAddLast:'
  def push
    # zero args variant does nothing
    self
  end

  # Associate: Search through self (an array of arrays).  Return first
  # array whose second element matches +key+ (using +key.==+).
  def rassoc(key)
    __assoc(key, 1)
  end


  primitive '__replace', 'rubyReplace:'

  def replace(arg)
    arg = Maglev::Type.coerce_to(arg, Array, :to_ary)
    self.__replace(arg)
  end

  primitive 'reverse', 'reverse'  # returns copy without copying named IVs

  def reverse!
    low = 0
    high = self.__size - 1
    while low < high
      a = self.__at(low)
      b = self.__at(high)
      self.__at_put(high, a)
      self.__at_put(low, b)
      low = low + 1
      high = high - 1
    end
    self
  end

  def reverse_each(&block)
    unless block_given?
      return ArrayReverseEnumerator.new(self, :reverse_each) # for 1.8.7
    end
    sz = self.__size
    i = sz - 1
    while i >= 0
      block.call(self.__at(i))
      new_siz = self.__size
      if new_siz < sz && i > 0
        sz = new_siz
        i = sz - 1
      else
        i -= 1
      end
    end
    self
  end

  
  def rindex(a1=MaglevUndefined, &block) # added for 1.8.7
    if a1._equal?(MaglevUndefined)
      i = self.__size - 1
      while i >= 0
        if block.call(self.__at(i))
          return i
        end
        i -= 1
        sz = self.__size
        if i >= sz
          i = sz - 1  # adjust for array shrinkage during iteration
        end
      end
      nil
    else
      self.rindex(a1)
    end
  end

  def rindex(element)
    i = self.__size - 1
    while i >= 0
      if self.__at(i) == element
        return i
      end
      i -= 1
    end
    nil
  end

  primitive_nobridge '__remove_from_to_', 'removeFrom:to:'

  # Let's get the basic use case working - just pulling a single random element
  # Check out https://github.com/rubinius/rubinius/blob/master/kernel/common/array.rb#L1286 for
  # the Rubinius implementation
  def sample(count=MaglevUndefined, options=MaglevUndefined)
    return at Kernel.rand(size)
  end

  def shift(count=MaglevUndefined)  # added in 1.8.7
    if count._equal?(MaglevUndefined) 
      return self.__shift
    end
    cnt = Maglev::Type.coerce_to(count, Fixnum, :to_int)
    if cnt < 0
      raise ArgumentError, 'arg to shift must be >= 0'
    end
    sz = self.__size
    if sz._equal?(0) || cnt._equal?(0)
      return []
    end
    if sz < cnt
      res = self.dup
      self.__size=(0)
      return res
    end
    res = self.__at(0, cnt)
    self.__remove_from_to_(1, cnt)  # one-based args
    return res
  end

  def shift
    self.__shift
  end

  def __shift
    sz = self.__size
    elem = nil
    unless sz._equal?(0)
      elem = self.__at(0)
      self.__remove_from_to_(1, 1)  # one-based args
    end
    elem
  end

  primitive 'size'
  primitive 'size=', 'size:'
  primitive '__size=', 'size:'

  def slice(*args)
    len = args.__size
    if len._equal?(1)
      slice(args.__at(0))
    elsif len._equal?(2)
      slice(args[0], args[1])
    else
      raise ArgumentError, 'expected 1 or 2 args'
    end
  end

  primitive_nobridge 'slice', '_rubyAt:'
  primitive_nobridge 'slice', '_rubyAt:length:'

  def slice!(start, length)
    start = Maglev::Type.coerce_to(start, Fixnum, :to_int)
    length = Maglev::Type.coerce_to(length, Fixnum, :to_int)
    my_siz = self.__size
    if my_siz._equal?(0)
      return self.class.__alloc(0, nil)
    end
    if start < 0
      start = my_siz + start
      if start < 0 # 1.8.7
        return nil
      end
    else
      if start >= my_siz  # 1.8.7, no change to self if start out of bounds
  if start._equal?(my_siz)
    return []
  end
  return nil
      end
    end
    result = self.__at(start, length)
    if result._not_equal?(nil) && length._not_equal?(0)
      self.__at_put(start, length , [] )
    end
    result
  end

  def slice!(arg)
    if arg._isRange
      start = Maglev::Type.coerce_to(arg.begin, Fixnum, :to_int)
      if start < 0
        my_siz = self.__size
        start = my_siz + start
        if start < 0 # 1.8.7
          return nil
        end
      end
      last = Maglev::Type.coerce_to(arg.end, Fixnum, :to_int)
      if last < 0
        last = self.__size + last
      end
      len = last - start + 1
      if arg.exclude_end?
        len -= 1
      end
      self.slice!(start,  len)
    else
      self.delete_at(arg)
    end
  end

  # Note: sort is listed in the Pick Axe book under both Array and Enumerable
  #  Smalltalk sort: expects a block returning boolean result of a <= b
  #

  primitive_nobridge '__sort!&', '_rubySort:'  # Smalltalk handles RubyBreakException

  def sort!(&block)
    if block_given?
      __sort!{ | a, b| block.call(a, b) <= 0 }
    else
      __sort!{ | a, b| (a <=> b) <= 0 }
    end
  end

  def sort(&block)
    d = dup
    if block_given?
      d.__sort!{ | a, b| block.call(a, b) <= 0 }
    else
      d.__sort!{ | a, b| (a <=> b) <= 0 }
    end
  end

  # sort_by: Do NOT implement an optimized version.  Pick up the
  # Enumerable#sort_by impl
  #
  # sort_by is required toimplement the Schwartzian Transform.  See pick
  # axe Enumerable#sort_by for a full discussion.

  def take(count) # added in 1.8.7
    # return an Array containing self[0 .. count-1 ]
    cnt = Maglev::Type.coerce_to(count, Fixnum, :to_int)
    if cnt < 0
      raise ArgumentError, 'arg to take must be >= 0'
    end
    return self.__at(0, cnt)
  end

  def take_while(&block)
    unless block_given?
      return ArrayEnumerator.new(self, :take_while)  # for 1.8.7
    end
    sz = self.__size
    broke = false
    n = 0
    enum_res = self.each { |elem|
      broke = true
      blk_res = block.call(elem)
      broke = false
      unless blk_res
        break
      end
      n += 1
    }
    if broke
      return enum_res  # the argument block did a break
    end
    self.take(n)
  end

  def to_a
    if self.class._equal?(Array)
      self
    else
      Array.new( self.to_ary )
    end
  end

  def to_ary
    self
  end

  def to_s
    '[' + self.collect { |v| v.inspect }.join(', ') + ']'
  end

  # Transpose rows and columns (assumes self is an array of arrays, all of
  # the same length).  If self is not an array of Arrays, then we should
  # raise a TypeError trying to convert an element to an array.
  def transpose
    my_size = self.__size
    result = []
    n = 0
    el_size = 0
    while n < my_size
      elem = Maglev::Type.coerce_to( self.__at(n), Array, :to_ary)
      elem_size = elem.__size
      if n._equal?(0)
        el_size = elem_size
      elsif elem_size._not_equal?(el_size)
  raise IndexError, 'All contained arrays must be same length'
      end
      j = 0
      while j < el_size
        res_elem =  result[j]
        if res_elem._equal?(nil)
          res_elem = []
          result[j] = res_elem
        end
        res_elem[n] = elem[j]
        j += 1
      end
      n += 1
    end
    result
  end

  def uniq
    hash = {}
    ary = self.__basic_dup_named_ivs
    i = 0
    lim = self.__size
    while i < lim
      el = self.__at(i)
      unless hash.include? el
        ary.__push( el )
        hash[el] = el
      end
      i = i + 1
    end
    ary
  end

  def uniq!
    old_size = self.__size
    r = uniq
    if old_size._equal?(r.__size)
      nil
    else
      # Only try to replace if size changed.  This prevents
      # unwanted frozen exception if there was no real change.
      replace(uniq)
    end
  end

  # Prepend elements to self.  If no elements, return unmodified self.
  def unshift(*elements)
    __insertall_at(elements, 1)
    self
  end

  def values_at(*selectors)
    # selectors is an Array
    lim = selectors.__size
    n = 0
    res = []
    while (n < lim)
      idx = selectors[n]
      if idx._isRange
        b_arr = idx.__beg_len(self.__size)
        if b_arr._not_equal?(nil)
          beg = b_arr.__at(0)
          len = b_arr.__at(1)
          j = 0
          while j < len
            res.__push( self.__at(j+beg) )
            j += 1
          end
        end
      else
        res.push(self.__at(idx))
      end
      n = n + 1
    end
    res
  end
  
  def self.try_convert(obj)
    begin
      array = Maglev::Type.__coerce_to_Array_to_ary(obj)
    rescue TypeError
      return nil
    end
    return array
  end

end
