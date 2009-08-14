class Array
  # begin private helper methods
  # TODO: Some of these don't begin with an '_'...

  primitive_nobridge '_at' , '_rubyAt:'
  primitive_nobridge '_at' , '_rubyAt:length:'

  primitive_nobridge '_at_put', '_rubyAt:put:'
  primitive_nobridge '_at_put', '_rubyAt:length:put:'

  primitive_nobridge '_fillFromToWith', 'fillFrom:to:with:'

  primitive 'size=', 'size:'

  # TODO consider a method prefix __nobr__ which during load prims
  #   would suppress generation of bridge methods for private
  #   methods that should not need bridge methods

  def _anySatisfyCaseLeaf( obj )
    # used in implementation of  while *list   within a   case
    n = 0
    lim = size
    while n < lim
      el = self._at(n)
      if obj === el
        return true
      end
      n = n + 1
    end
    false
  end

  def _anySatisfyCaseTrue
    # used in implementation of  while *list   within a   case
    n = 0
    lim = size
    while n < lim
      el = self._at(n)
      if el
        return true
      end
      n = n + 1
    end
    false
  end

  # Used by both assoc and rassoc, since they differ only in the index of
  # the array to compare to the key
  def _assoc(key, idx)
    i = 0
    lim = self.size
    while i < lim
      el = self._at(i)
      if el._isArray && el[idx] == key
        return el
      end
      i += 1
    end
    nil
  end

  def _as_hash
    # called from generated code
    Hash[self]
  end

  primitive_nobridge '_insertall_at', 'insertAll:at:'

  def _add_arguments(arg)
    # called from generated code
    if arg._isArray
      if arg.size._not_equal?(0)
        self._insertall_at(arg, self.size + 1)
      end
    else
      a = arg._splat_lasgn_value_coerce
      if a._isArray
        if a.size._not_equal?(0)
          self._insertall_at(a, self.size + 1)
        end
      else
        self << a
      end
    end
    self
  end

  def _flatten_onto(output)
    # returns true if any recursion done into child arrays
    recursed = false
    ts = Thread._recursion_guard_set
    added = ts._add_if_absent(self)
    unless added
      # self was already in the set
      raise ArgumentError, 'recursive element in flatten'
    end
    begin
      i = 0
      lim = size
      while i < lim
        el = self._at(i)
        if el._isArray
          el._flatten_onto(output)
          recursed = true
        elsif el.respond_to?(:to_ary)
          el = el.to_ary
          if el._isArray
            el._flatten_onto(output)
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
  class_primitive_nobridge '_alloc', '_rubyNew:initValue:'
  class_primitive_nobridge '_withall', '_rubyWithAll:'

  # Returns a new array with the given elements.
  def self.[](*elements)
    _withall(elements)
  end

  # implementations of new need to call initialize in case application has
  #   subclassed Array and reimplemented initialize .
  def self.new(*args)
    # this variant gets bridge methods
    len = args.length
    if len <= 2
      if  len.equal?(2)
        a = self.new(args[0], args[1])
      elsif len.equal?(1)
        a = self.new(args[0])
      elsif len.equal?(0)
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
    if len.equal?(0)
      self
    elsif len.equal?(1)
      _initialize(args[0], &blk)
    elsif len.equal?(2)
      _initialize(args[0], args[1])
    else
      raise ArgumentError, 'too many args'
    end
  end

  def self.new(first, &blk)
    if first._isArray
      a = _withall(first) # ignore the block
    else
      siz = Type.coerce_to(first, Fixnum, :to_int)
      a = _alloc(siz, nil)
      # blk processed by initialize
    end
    a.initialize(first, &blk)
    a
  end

  def initialize(first, &blk)
    _initialize(first, &blk)
  end

  def _initialize(first, &blk)
    if self.class.equal?(Array)
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
        self._at_put(n,  blk.call(n))
        n = n + 1
      end
    end
    self
  end


  def self.new(a_size, value)
    if self.equal?(Array)
      s = Type.coerce_to(a_size, Fixnum, :to_int)
      a = _alloc(s, value)
    else
      a = _alloc(0, nil)
    end
    a.initialize(a_size, value)
    a
  end

  def initialize(a_size, value)
    if self.class.equal?(Array)
      # do nothing
    else
      s = Type.coerce_to(a_size, Fixnum, :to_int)
      self.size=(s)
      self.fill(value, 0, s)
    end
    self
  end
  def _initialize(a_size, value)
    if self.class.equal?(Array)
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
    if self.equal?(Array)
      if arg._isFixnum
        a = _alloc(arg, nil)
        return a
      elsif arg._isArray
        a = _alloc(arg.size, nil)
      else
        arg = _coerce_one_arg(arg)
        if arg._isFixnum
          a = _alloc(arg, nil)
          return a
        else
          a = _alloc(arg.size, nil)
        end
      end
    else
      a = _alloc(0, nil)
    end
    a.initialize(arg)
  end

  def self._coerce_one_arg(arg)
    begin
      carg = Type.coerce_to(arg, Array, :to_ary)
    rescue TypeError
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
      arg = Array._coerce_one_arg(arg)
      if arg._isFixnum
        self.size=(arg)
      else
        self.replace(arg)
      end
    end
    self
  end

  def self.new
    a = _alloc(0, nil)
    a.initialize
    a
  end

  def initialize
    self.size=(0)
    self
  end

  def self.new(&blk)
    # ignores the block
    a = _alloc(0, nil)
    a.initialize(&blk)
  end

  def initialize(&blk)
    self.size=(0)
    self
  end


  # Set intersection. 
  # Return new array containing elements common to two arrays.
  def &(other)
    other = Type.coerce_to(other, Array, :to_ary)
    my_siz = self.size
    other_siz  = other.size
    dflt = Object.new
    htsiz = (my_siz + other_siz) / 4
    htsiz = 5 if htsiz < 5  
    dict = Hash._new(htsiz)
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
      elem = self._at(n)
      if dict._delete_otherwise(elem, dflt)._not_equal?(dflt)
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
      res._insertall_at(arg, res.size + 1)
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
      el = self._at(i)
      if (h[el].equal?(default))
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
    # TODO: need to get a coercion idiom going...
    #
    # [1] <=> "a" Should throw a TypeError: can't convert String into Array.
    # but, "a".to_a => ["a"]...

    # TODO: need to throw a type error in many(?) situations, certainly
    # with strings....

    # TODO: [1,2,3] <=> ["a"] should return nil, but throws a No method
    # for #'_generality....

    other = Type.coerce_to(other, Array, :to_ary)
    ts = Thread._recursion_guard_set
    added = ts._add_if_absent(self)
    begin
      other_size = other.size
      my_size = size
      lim = my_size > other_size ? other_size : my_size # lim is the min
      i = 0
      while i < lim
        curr = self._at(i)
        if ts.include?(curr)
          unless curr.equal?(other[i])
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
    return true if equal?(other)
    return false unless other._isArray
    lim = self.size
    unless lim.equal?(other.size)
      return false
    end
    ts = Thread._recursion_guard_set
    added = ts._add_if_absent(self)
    begin
      i = 0
      limi = lim
      while i < limi
        v = self._at(i)
        ov = other[i]
        if v.equal?(ov)
    # ok
        elsif ts.include?(v) || ts.include?(ov)
          if v.equal?(self) && ov.equal?(other)
            # ok
          elsif v.equal?(other) && ov.equal?(self)
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
    return true if self.equal?(other)
    return false unless other._isArray
    lim = self.size
    return false unless lim.equal?(other.size)
    ts = Thread._recursion_guard_set
    added = ts._add_if_absent(self)
    begin
      i = 0
      limi = lim
      while i < limi
        v = self._at(i)
        ov = other[i]
        if v.equal?(ov)
          # ok
        elsif ts.include?(v) || ts.include?(ov)
          if v.equal?(self) && ov.equal?(other)
            # ok
          elsif v.equal?(other) && ov.equal?(self)
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

  def [](*args)
    len = args.size
    if len.equal?(1)
      slice(args[0])
    elsif len.equal?(2)
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
      el = self._at(i)
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
    _assoc(key, 0)
  end

  primitive 'at' , '_rubyAt:'
  primitive 'clear', 'removeAll'

  def collect!(&b)
    i = 0
    lim = size
    while i < lim
      self._at_put(i,  b.call(self._at(i) ) )
      i += 1
    end
    self
  end

  primitive_nobridge '_copy_from_to', 'copyFrom:to:'

  def _copy_delete_last(count)
    # Returns an Array containing the last  count  elements of receiver
    # and deletes those elements  from the receiver.
    if count.equal?(0)
      res = []
    else
      sz = self.size
      res = self._copy_from_to( sz - count + 1 , sz )
      self.size=( sz - count  )
    end
    res
  end

  # Return copy of self with all nil elements removed
  def compact
    result = self.class.new
    i = 0
    lim = size
    while i < lim
      el = self._at(i)
      result << el unless el.equal?(nil)
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
      break if self._at(i).equal?(nil)
      i += 1
    end
    return nil if i.equal?(lim)

    fill_idx = i
    while i < lim
      el = self._at(i)
      unless el.equal?(nil)
        self._at_put(fill_idx,  el )
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
      self._insertall_at(arg, self.size + 1)
    end
    self
  end

  def delete(obj)
    n = self.size - 1
    res = nil
    while n >= 0
      if self._at(n) == obj
        oidx = n + 1
        self._remove_from_to_(oidx, oidx)
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
      if self._at(n) == obj
        oidx = n + 1
        self._remove_from_to_(oidx, oidx)
        found = true
      end
      n = n - 1
    end
    if found.equal?(false)
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
    elem = self._at(idx)
    oidx = idx + 1
    self._remove_from_to_(oidx, oidx)
    return elem
  end

  # Delete every element of self for which +block+ evalutes to +true+.
  def delete_if(&block)
    i = 0
    lim = size
    while i < lim
      break if block.call(self._at(i) )
      i += 1
    end

    fill_idx = i
    while i < lim
      el = self._at(i)
      unless block.call(el)
        self._at_put(fill_idx,  el)
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
      b.call(self._at(i))
      i += 1
    end
    self
  end

  def each_with_index(&b)
    i = 0
    lim = size
    while i < lim
      b.call(self._at(i), i)
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
    if bad.equal?(true)
      if block_given?
        return blk.call(index)
      else
        raise IndexError , 'offset out of bounds'
      end
    end
    self._at(idx)
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
    self._at(idx)
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
    self._at(idx)
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
    if start.equal?(nil)
      start = 0
    else
      start = Type.coerce_to(start, Fixnum, :to_int)
    end

    sz = self.size

    unless length._isFixnum
      unless length.equal?(nil)
        if (length.kind_of?(Bignum))
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

    if length.equal?(nil)
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
      self.size=(endIdx)  # grow the receiver
    end
    if (length > 0)
      _fillFromToWith(start, endIdx, obj)
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
      if start.equal?(nil)
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

    if length.equal?(nil)
      length = sz - start
    else
      unless length._isFixnum
        length = length.to_int
      end
    end
    n = start
    limit = start + length
    while (n < limit)
      self._at_put(n, blk.call(n) )
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
    self._at(0)
  end

  def first(count)
    cnt = Type.coerce_to(count, Fixnum, :to_int)
    if cnt < 0
      raise ArgumentError, 'negative count'
    end
    self._at(0, cnt)
  end

  def flatten
    ary = self.class.new
    _flatten_onto( ary )
    ary
  end

  def flatten!
    ary = []
    recursed = _flatten_onto(ary)
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
      if self._at(n) == obj
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
      if self._at(i) == el
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
    self._at(ofs, cnt)
  end

  def last
    my_size = self.size
    if my_size.equal?(0)
      return nil
    end
    return self._at(my_size - 1)
  end

  primitive 'length', 'size'

  alias map! collect!

  # Number of non-<tt>nil</tt> elements in self.
  def nitems
    count = 0
    i = 0
    lim = size
    while i < lim
      count += 1 unless self._at(i).equal?(nil)
      i += 1
    end
    count
  end

  primitive 'pack', 'rubyPack:'

  def self._pack_coerce(obj, sym)
    begin
      r = obj.__send__(sym)
    rescue Exception
      return nil  # capiprim.c will raise TypeError
    end
    r
  end

  def pop
    sz = self.size
    unless sz.equal?(0)
      idx = sz - 1
      elem = self._at(idx)
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
    _assoc(key, 1)
  end


  primitive '_replace', 'rubyReplace:'

  def replace(arg)
    arg = Type.coerce_to(arg, Array, :to_ary)
    self._replace(arg) 
  end

  primitive 'reverse', 'reverse'

  def reverse!
    low = 0
    high = self.size - 1
    while low < high
      a = self._at(low)
      b = self._at(high)
      self._at_put(high, a)
      self._at_put(low, b)
      low = low + 1
      high = high - 1
    end
    self
  end

  def reverse_each(&b)
    sz = self.size
    i = sz - 1
    while i >= 0
      b.call(self._at(i))
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
      if(self._at(i) == el)
        return i
      end
      i -= 1
    end
    nil
  end

  primitive_nobridge '_remove_from_to_', 'removeFrom:to:'

  def _remove_last(count)
    # deletes the last count elements of receiver.
    unless count.equal?(0)
      sz = self.size
      self.size=( sz - count  )
    end
    self
  end

  def shift
    sz = self.size
    unless sz.equal?(0)
      elem = self._at(0)
      _remove_from_to_(1, 1)
      elem
    end
  end

  primitive 'size'

  primitive 'slice', '_rubyAt:'
  primitive 'slice', '_rubyAt:length:'

  def slice(*args)
    len = args.size
    if len.equal?(1)
      slice(args._at(0))
    elsif len.equal?(2)
      slice(args[0], args[1])
    else
      raise ArgumentError, 'expected 1 or 2 args'
    end
  end

  def slice!(start, length)
    if self.size.equal?(0)
      return self.class.new
    end
    result = self._at(start, length)
    if result._not_equal?(nil)
      self._at_put(start, length , [] )
    end
    result
  end
  primitive 'slice!' , '_rubySliceEx:length:'  #  def slice!(start, length);end

  primitive '_slice_ex_range', '_rubySliceExRange:'

  def slice!(arg)
    if arg._isRange
      self._slice_ex_range(arg)
    else
      self.delete_at(arg)
    end
  end

  # Note: sort is listed in the Pick Axe book under both Array and Enumerable
  #  Smalltalk sort: expects a block returning boolean result of a <= b
  #
  primitive_nobridge '_sort!&', 'sort:'

  def sort!(&blk)
    if (block_given?)
      _sort!{ | a, b| blk.call(a, b) <= 0 }
    else
      _sort!{ | a, b| (a <=> b) <= 0 }
    end
  end

  def sort(&blk)
    d = dup
    if (block_given?)
      d._sort!{ | a, b| blk.call(a, b) <= 0 }
    else
      d._sort!{ | a, b| (a <=> b) <= 0 }
    end
    d
  end

  # sort_by: Do NOT implement an optimized version.  Pick up the
  # Enumerable#sort_by impl
  #
  # sort_by is required toimplement the Schwartzian Transform.  See pick
  # axe Enumerable#sort_by for a full discussion.

  def to_a
    if self.class.equal?(Array)
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
      elem = Type.coerce_to( self._at(n), Array, :to_ary)
      if n.equal?(0)
        el_size = elem.size 
      elsif elem.size._not_equal?(el_size)
	raise IndexError, 'All contained arrays must be same length'
      end
      j = 0
      while j < el_size
        res_elem =  result[j] 
        if res_elem.equal?(nil)
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
      el = self._at(i)
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
    if old_size.equal?(r.size)
      nil
    else
      # Only try to replace if size changed.  This prevents
      # unwanted frozen exception if there was no real change.
      replace(uniq)
    end
  end

  # Prepend elements to self.  If no elements, return unmodified self.
  def unshift(*elements)
    _insertall_at(elements, 1)
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
        beg, len = idx._beg_len(self.length)
        if beg
          j = 0
          while j < len
            res << self._at(j+beg)
            j += 1
          end
        end
      else
        res.push(self._at(idx))
      end
      n = n + 1
    end
    res
  end

  # clone, dup inherited from Object

  # Overrides from Object that are not documented in Array
  #   (e.g., eql? is documented in Array, so is not listed here).

  primitive 'hash', '_rubyHash'

  def inspect
    s = "["
    ts = Thread._recursion_guard_set
    added = ts._add_if_absent(self)
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

def Array(arg)
  arg.to_a rescue [arg]
end
