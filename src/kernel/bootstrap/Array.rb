class Array

  # TODO consider a method prefix __nobr__ which during load prims
  #   would suppress generation of bridge methods for private
  #   methods that should not need bridge methods

  def __anySatisfyCaseLeaf( obj )
    # used in implementation of  while *list   within a   case
    n = 0
    lim = size
    while n < lim
      el = self.__at(n)
      if obj === el
        return true
      end
      n = n + 1
    end
    false
  end

  def __anySatisfyCaseTrue
    # used in implementation of  while *list   within a   case
    n = 0
    lim = size
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
    lim = self.size
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
    Hash[self]
  end

  primitive_nobridge '__fill_resize', 'fillFrom:resizeTo:with:'
  primitive_nobridge '__insertall_at', 'insertAll:at:'

  def __add_arguments(arg)
    # called from generated code
    if arg._isArray
      if arg.size._not_equal?(0)
        self.__insertall_at(arg, self.size + 1)
      end
    else
      a = arg._splat_lasgn_value_coerce
      if a._isArray
        if a.size._not_equal?(0)
          self.__insertall_at(a, self.size + 1)
        end
      else
        self << a
      end
    end
    self
  end

  def __flatten_onto(output)
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
      lim = size
      while i < lim
        el = self.__at(i)
        if el._isArray
          el.__flatten_onto(output)
          recursed = true
        elsif el.respond_to?(:to_ary)
          el = el.to_ary
          if el._isArray
            el.__flatten_onto(output)
            recursed = true
          end
        else
          output << el
        end
        i = i + 1
      end
    ensure
      if added
        ts.remove(self)
      end
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
    len = args.length
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
      raise ArgumentError, 'too many args'
    end
  end

  def initialize(*args, &blk)
    # this variant gets bridge methods
    #
    # If a Subclass#initialize calls super(*args), then we go through this
    # variant.  That means we also have to unpack the parameters for all
    # the possible variants here.  The initialize variants below will be
    # called directly, in the case of Array, or of a subclass calling super
    # with explicit args, e.g. super(x,y).

    len = args.length
    if len._equal?(0)
      self
    elsif len._equal?(1)
      __initialize(args[0], &blk)
    elsif len._equal?(2)
      __initialize(args[0], args[1])
    else
      raise ArgumentError, 'too many args'
    end
  end

  def self.new(first, &blk)
    if first._isArray
      a = __withall(first) # ignore the block
    else
      siz = Type.coerce_to(first, Fixnum, :to_int)
      a = __alloc(siz, nil)
      # blk processed by initialize
    end
    a.initialize(first, &blk)
    a
  end

  def initialize(first, &blk)
    __initialize(first, &blk)
  end

  def __initialize(first, &blk)
    if self.class._equal?(Array)
      if first._isArray
        lim = 0 # ignore any block
      else
        lim = self.size
      end
    else
      if first._isArray
        self.replace(first)
        lim = 0 # ignore any block
      else
        lim = Type.coerce_to(first, Fixnum, :to_int)
        self.size=(lim)
      end
    end
    if block_given?
      n = 0
      while (n < lim)
        self.__at_put(n,  blk.call(n))
        n = n + 1
      end
    end
    self
  end


  def self.new(a_size, value)
    if self._equal?(Array)
      s = Type.coerce_to(a_size, Fixnum, :to_int)
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
      s = Type.coerce_to(a_size, Fixnum, :to_int)
      self.size=(s)
      self.fill(value, 0, s)
    end
    self
  end
  def __initialize(a_size, value)
    if self.class._equal?(Array)
      # do nothing
    else
      s = Type.coerce_to(a_size, Fixnum, :to_int)
      self.size=(s)
      self.fill(value, 0, s)
    end
    self
  end

  def self.new(first, second, &blk)
    if block_given?
      self.new(first, &blk)  # ignore second arg
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
        a = __alloc(arg.size, nil)
      else
        arg = __coerce_one_arg(arg)
        if arg._isFixnum
          a = __alloc(arg, nil)
          return a
        else
          a = __alloc(arg.size, nil)
        end
      end
    else
      a = __alloc(0, nil)
    end
    a.initialize(arg)
  end

  def self.__coerce_one_arg(arg)
    carg = Type.coerce_to_or_nil(arg, Array, :to_ary)
    if carg._equal?(nil)
      carg = Type.coerce_to(arg, Fixnum, :to_int)
    end
    carg
  end

  def initialize(arg)
    if arg._isFixnum
      self.size=(arg)
    elsif arg._isArray
      self.replace(arg)
    else
      arg = Array.__coerce_one_arg(arg)
      if arg._isFixnum
        self.size=(arg)
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
    self.size=(0)
    self
  end

  def self.new(&blk)
    # ignores the block
    a = __alloc(0, nil)
    a.initialize(&blk)
  end

  def initialize(&blk)
    self.size=(0)
    self
  end

  # double quoted string literals support

  def __joinStrings
    # called from generated code
    str = ''
    n = 0
    siz = self.size
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
    str = self.__joinStrings
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
    other = Type.coerce_to(other, Array, :to_ary)
    my_siz = self.size
    other_siz  = other.size
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
    res = self.class.new( my_siz < other_siz ? my_siz : other_siz )
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
    res.size=(res_idx)
    res
  end

  # Repetition: if +obj+ is a string, then <tt>arr.join(str)</tt>, otherwise
  # return a new array by concatenating +obj+ copies of self.
  def *(obj)
    if obj.respond_to?( :to_str )
      return join(obj)
    end
    val = Type.coerce_to(obj, Fixnum, :to_int)
    if val < 0
      raise ArgumentError, "arg must be >= 0"
    end
    result = self.class.new
    i = 0
    while i < val
      result.concat(self)
      i += 1
    end
    result
  end

  def +(arg)
    arg = Type.coerce_to(arg, Array, :to_ary)
    res = self.dup
    if arg.size._not_equal?(0)
      res.__insertall_at(arg, res.size + 1)
    end
    res
  end

  def -(arg)
    arg = Type.coerce_to(arg, Array, :to_ary)
    argSize = arg.size
    mySize = size
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
      if (h[el]._equal?(default))
        res << el
      end
      i = i + 1
    end
    res
  end

  # note, <<  can't use smalltalk add: , it returns arg, not receiver
  primitive '<<', '_rubyAddLast:'

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
    other = Type.coerce_to(other, Array, :to_ary)
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    begin
      other_size = other.size
      my_size = size
      lim = my_size > other_size ? other_size : my_size # lim is the min
      i = 0
      while i < lim
        curr = self.__at(i)
        if ts.include?(curr)
          unless curr._equal?(other[i])
            return 1 if size > other_size
            return -1 if size < other_size
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

  def ==(other)
    return true if self._equal?(other)
    return false unless other._isArray
    lim = self.size
    unless lim._equal?(other.size)
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
          if v._equal?(self) && ov._equal?(other)
            # ok
          elsif v._equal?(other) && ov._equal?(self)
            # ok
          else
            raise ArgumentError, 'recursion too complex for Array#=='
          end
        elsif v == ov
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

  # Return true if both are the same object, or if both are arrays, and
  # have the same number of elements and all corresponding elements are
  # eql?.
  #
  def eql?(other)
    return true if self._equal?(other)
    return false unless other._isArray
    lim = self.size
    return false unless lim._equal?(other.size)
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
          if v._equal?(self) && ov._equal?(other)
            # ok
          elsif v._equal?(other) && ov._equal?(self)
            # ok
          else
            raise ArgumentError, 'recursion too complex for Array#=='
          end
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
    len = args.size
    if len._equal?(1)
      slice(args[0])
    elsif len._equal?(2)
      slice(args[0], args[1])
    else
      raise ArgumentError, 'expected 1 or 2 args'
    end
  end

  primitive_nobridge '[]=', '_rubyAt:put:'
  primitive_nobridge '[]=', '_rubyAt:length:put:'

  # Set Union.  Removes duplicates from self: [1,1,1] | [] => [1]
  def |(other)
    other = Type.coerce_to(other, Array, :to_ary)
    hash = {}
    ary = []
    i = 0
    lim = size
    while i < lim
      el = self.__at(i)
      unless hash.include? el
        ary << el
        hash[el] = el
      end
      i = i + 1
    end

    i = 0
    lim = other.size
    while i < lim
      el = other[i]
      unless hash.include? el
        ary << el
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
  primitive 'clear', 'removeAll'

  def collect!(&b)
    i = 0
    lim = size
    while i < lim
      self.__at_put(i,  b.call(self.__at(i) ) )
      i += 1
    end
    self
  end

  primitive_nobridge '__copy_from_to', 'copyFrom:to:'

  # Return copy of self with all nil elements removed
  def compact
    result = self.class.new
    i = 0
    lim = size
    while i < lim
      el = self.__at(i)
      result << el unless el._equal?(nil)
      i += 1
    end
    result
  end

  # Remove all nil elements from self.  Return nil if no changes,
  # otherwise return self.
  def compact!
    i = 0
    lim = size
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
    self.size = fill_idx
    self
  end

  primitive_nobridge 'concat*', '_rubyAddAll:'

  def concat(arg)
    arg = Type.coerce_to(arg, Array, :to_ary)
    if arg.size._not_equal?(0)
      self.__insertall_at(arg, self.size + 1)
    end
    self
  end

  def delete(obj)
    n = self.size - 1
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

  def delete(obj, &blk)
    n = self.size - 1
    found = false
    lim = self.size
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
        blk.call
      else
        nil
      end
    else
      obj
    end
  end


  # Delete element at specified +index+.  Return the deleted item, or
  # +nil+ if no item at +index+
  def delete_at(idx)
    idx = Type.coerce_to(idx, Fixnum, :to_int)
    sz = self.size
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
    i = 0
    lim = size
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
    self.size=(fill_idx)
    self
  end

  def each(&b)
    i = 0
    lim = size
    while i < lim
      b.call(self.__at(i))
      i += 1
    end
    self
  end

  def each_with_index(&b)
    i = 0
    lim = size
    while i < lim
      b.call(self.__at(i), i)
      i += 1
    end
    self
  end

  def each_index(&b)
    0.upto(size-1, &b)
    self
  end

  primitive 'empty?', 'isEmpty'

  def fetch(index, &blk)
    # this variant gets bridge methods
    idx = Type.coerce_to(index, Fixnum, :to_int)
    my_siz = self.length
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
        return blk.call(index)
      else
        raise IndexError , 'offset out of bounds'
      end
    end
    self.__at(idx)
  end

  def fetch(index)
    idx = Type.coerce_to(index, Fixnum, :to_int)
    my_siz = self.length
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
    idx = Type.coerce_to(index, Fixnum, :to_int)
    my_siz = self.length
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

  def fetch(index, default, &blk)
    if block_given?
      self.fetch(index, &blk)
    else
      self.fetch(index, default)
    end
  end

  #  note multiple variants below
  def fill(obj, start=nil, length=nil)
    if start._equal?(nil)
      start = 0
    else
      start = Type.coerce_to(start, Fixnum, :to_int)
    end

    sz = self.size

    unless length._isFixnum
      unless length._equal?(nil)
        if (length._kind_of?(Bignum))
          raise RangeError, "#{length} too big" if length >= 2**63
        else
          length = Type.coerce_to(length, Fixnum, :to_int)
        end
        raise ArgumentError if length > sz
      end
    end

    if (start < 0)
      start = sz + start
      if (start < 0)
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
    if (endIdx > sz)
      if length > 0
        __fill_resize(start, endIdx, obj) # resize and fill 
      else
        self.size=(endIdx)  # grow the receiver
      end
    elsif (length > 0)
      __fill_resize(start, 0 - endIdx, obj) # fill without resize
    end
    self
  end

  def fill(obj, start)
    # note no bridge methods for second and later variants
    if (start._isRange)
      s = Type.coerce_to(start.begin, Fixnum, :to_int)
      e = Type.coerce_to(start.end,   Fixnum, :to_int)
      s += self.size if s < 0
      e += self.size if e < 0
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

  def fill(start=nil, length=nil , &blk)
    # note no bridge methods for second and later variants
    unless start._isFixnum
      if start._equal?(nil)
        start = 0
      else
        start = start.to_int
      end
    end

    sz = self.size
    if (start < 0)
      start = sz + start
      if (start < 0)
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
    while (n < limit)
      self.__at_put(n, blk.call(n) )
      n = n + 1
    end
    self
  end

  def fill(&blk)
    # note no bridge methods for second and later variants
    fill(nil, nil, &blk)
    self
  end

  def fill(start, &blk)
    # note no bridge methods for second and later variants
    if (start._isRange)
      s = Type.coerce_to(start.begin, Fixnum, :to_int)
      e = Type.coerce_to(start.end,   Fixnum, :to_int)
      s += self.size if s < 0
      e += self.size if e < 0
      if start.exclude_end?
        return self if s == e
        e -= 1
      end
      raise RangeError, "#{start.inspect} out of range" if s < 0
      return self if e < 0
      s.upto(e) do | n |
        self[n] = yield n
      end
    else
      fill(start, nil, &blk)
    end
    self
  end

  def fill(a, b, c, *d)
    if (d.length > 0)
      raise ArgumentError , 'too many args'
    else
      return fill(a, b, c)
    end
  end

  def fill(a, b, c, &blk)
    raise ArgumentError , 'too many args'
  end

  def first
    self.__at(0)
  end

  def first(count)
    cnt = Type.coerce_to(count, Fixnum, :to_int)
    if cnt < 0
      raise ArgumentError, 'negative count'
    end
    self.__at(0, cnt)
  end

  def flatten
    ary = self.class.new
    __flatten_onto( ary )
    ary
  end

  def flatten!
    ary = []
    recursed = __flatten_onto(ary)
    if recursed
      self.replace(ary)
      self
    else
      nil
    end
  end

  # Note: The Pick Axe book has this method documented under both Array and
  # Enumerable.
  def include?(obj)
    n = 0
    lim = self.size
    while n < lim
      if self.__at(n) == obj
        return true
      end
      n += 1
    end
    false
  end

  def index(el)
    i = 0
    lim = size
    while i < lim
      if self.__at(i) == el
        return i
      end
      i += 1
    end
    nil
  end

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
    return self if args.length == 0
    idx = Type.coerce_to(idx, Fixnum, :to_int)
    idx += (size + 1) if idx < 0
    raise IndexError, "#{idx} out of bounds" if idx < 0
    self[idx, 0] = args
    self
  end

  def last(count)
    cnt = Type.coerce_to(count, Fixnum, :to_int)
    my_size = self.size
    if cnt <= 0
      if cnt < 0
        raise ArgumentError, 'count must be >= 0'
      end
      return self.class.new
    end
    ofs = my_size - cnt
    if ofs < 0
      ofs = 0
    end
    self.__at(ofs, cnt)
  end

  def last
    my_size = self.size
    if my_size._equal?(0)
      return nil
    end
    return self.__at(my_size - 1)
  end

  primitive 'length', 'size'

  alias map! collect!

  # Number of non-<tt>nil</tt> elements in self.
  def nitems
    count = 0
    i = 0
    lim = size
    while i < lim
      count += 1 unless self.__at(i)._equal?(nil)
      i += 1
    end
    count
  end

  primitive 'pack', 'rubyPack:'

  def self.__pack_coerce(obj, sym)
    # sent from C code in capiprim.c 
    begin
      r = obj.__send__(sym)
    rescue Exception
      return nil  # capiprim.c will raise TypeError
    end
    r
  end

  def pop
    sz = self.size
    unless sz._equal?(0)
      idx = sz - 1
      elem = self.__at(idx)
      self.size=(idx)
      elem
    end
  end

  primitive 'push*', '_rubyAddAll:'
  primitive_nobridge 'push', '_rubyAddLast:'
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
    arg = Type.coerce_to(arg, Array, :to_ary)
    self.__replace(arg) 
  end

  primitive 'reverse', 'reverse'

  def reverse!
    low = 0
    high = self.size - 1
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

  def reverse_each(&b)
    sz = self.size
    i = sz - 1
    while i >= 0
      b.call(self.__at(i))
      new_siz = self.size
      if new_siz < sz && i > 0
        sz = new_siz
        i = sz - 1
      else
        i -= 1
      end
    end
    self
  end

  def rindex(el)
    i = size - 1
    while i >= 0
      if(self.__at(i) == el)
        return i
      end
      i -= 1
    end
    nil
  end

  primitive_nobridge '__remove_from_to_', 'removeFrom:to:'

  def shift
    sz = self.size
    unless sz._equal?(0)
      elem = self.__at(0)
      __remove_from_to_(1, 1)
      elem
    end
  end

  primitive 'size'
  primitive 'size=', 'size:'

  primitive 'slice', '_rubyAt:'
  primitive 'slice', '_rubyAt:length:'

  def slice(*args)
    len = args.size
    if len._equal?(1)
      slice(args.__at(0))
    elsif len._equal?(2)
      slice(args[0], args[1])
    else
      raise ArgumentError, 'expected 1 or 2 args'
    end
  end

  def slice!(start, length)
    if self.size._equal?(0)
      return self.class.new
    end
    result = self.__at(start, length)
    if result._not_equal?(nil)
      self.__at_put(start, length , [] )
    end
    result
  end
  primitive 'slice!' , '_rubySliceEx:length:'  #  def slice!(start, length);end

  primitive '__slice_ex_range', '_rubySliceExRange:'

  def slice!(arg)
    if arg._isRange
      self.__slice_ex_range(arg)
    else
      self.delete_at(arg)
    end
  end

  # Note: sort is listed in the Pick Axe book under both Array and Enumerable
  #  Smalltalk sort: expects a block returning boolean result of a <= b
  #
  primitive_nobridge '__sort!&', 'sort:'

  def sort!(&blk)
    if (block_given?)
      __sort!{ | a, b| blk.call(a, b) <= 0 }
    else
      __sort!{ | a, b| (a <=> b) <= 0 }
    end
  end

  def sort(&blk)
    d = dup
    if (block_given?)
      d.__sort!{ | a, b| blk.call(a, b) <= 0 }
    else
      d.__sort!{ | a, b| (a <=> b) <= 0 }
    end
    d
  end

  # sort_by: Do NOT implement an optimized version.  Pick up the
  # Enumerable#sort_by impl
  #
  # sort_by is required toimplement the Schwartzian Transform.  See pick
  # axe Enumerable#sort_by for a full discussion.

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
    self.join
  end

  # Transpose rows and columns (assumes self is an array of arrays, all of
  # the same length).  If self is not an array of Arrays, then we should
  # raise a TypeError trying to convert an element to an array.
  def transpose
    my_size = self.size
    result = []
    n = 0
    el_size = 0
    while n < my_size
      elem = Type.coerce_to( self.__at(n), Array, :to_ary)
      if n._equal?(0)
        el_size = elem.size 
      elsif elem.size._not_equal?(el_size)
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
    ary = self.class.new # Ensure we return proper subclass
    i = 0
    lim = size
    while i < lim
      el = self.__at(i)
      unless hash.include? el
        ary << el
        hash[el] = el
      end
      i = i + 1
    end
    ary
  end

  def uniq!
    old_size = size
    r = uniq
    if old_size._equal?(r.size)
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
    # selectors is an Array of
    lim = selectors.size
    n = 0
    res = []
    while (n < lim)
      idx = selectors[n]
      if (idx._isRange)
        beg, len = idx.__beg_len(self.length)
        if beg
          j = 0
          while j < len
            res << self.__at(j+beg)
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

  # clone, dup inherited from Object

  # Overrides from Object that are not documented in Array
  #   (e.g., eql? is documented in Array, so is not listed here).

  def hash
    # sample at most 5 elements of receiver
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added
      return 0 
    end
    hval = 4459 
    begin
      mysize = self.size
      interval = (mysize - 1).__divide(4)
      if interval < 1
        interval = 1
      end
      n = 0
      while n < mysize
        elem = self.__at(n)
        eh = elem.hash
        if eh._not_equal?(0)
          eh = Type.coerce_to( eh, Fixnum, :to_int)
          hval = (hval >> 1) ^ eh
        end
        n += interval
      end
    ensure
      ts.remove(self)
    end
    hval 
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

end
