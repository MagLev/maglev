class Array
  # begin private helper methods
  # TODO: Some of these don't begin with an '_'...

  primitive_nobridge '_at' , '_rubyAt:'
  primitive_nobridge '_at' , '_rubyAt:length:'

  primitive_nobridge '_at_put', '_rubyAt:put:'
  primitive_nobridge '_at_put', '_rubyAt:length:put:'

  primitive_nobridge '_fillFromToWith', 'fillFrom:to:with:'
  primitive_nobridge 'insert_all', 'insertAll:at:'

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
    lim = size
    while i < lim
      el = self._at(i)
      return el if el[idx] == key
      i += 1
    end
    nil
  end

  # TODO: This should be a private _ method, right?
  def flatten_onto(output)
    i = 0
    lim = size
    while i < lim
      el = self._at(i)
      el.flatten_onto(output)
      i = i + 1
    end
    output
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

  def initialize(*args)
    # this variant gets bridge methods
    raise ArgumentError, 'too many args'
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
      elsif arg._isArray
        a = _withall(arg)
      else
        a = _alloc_one_arg(arg)
      end
    else
      a = _alloc(0, nil)
    end
    a.initialize(arg)
    a
  end

  def self._alloc_one_arg(arg)
    begin
      ary = Type.coerce_to(arg, Array, :to_ary)
      a = _withall(ary)
    rescue TypeError
      siz = Type.coerce_to(arg, Fixnum, :to_int)
      a = _alloc(siz, nil)
    end
    a
  end

  def _init_one_arg(arg)
    begin
      ary = Type.coerce_to(arg, Array, :to_ary)
      self.replace(arg)
    rescue TypeError
      siz = Type.coerce_to(arg, Fixnum, :to_int)
      self.size=(siz)
    end
    self
  end

  def initialize(arg)
    if self.class.equal?(Array)
      # do nothing
    else
      if arg._isFixnum
        self.size=(arg)
      elsif arg._isArray
        self.replace(arg)
      else
        _init_one_arg(arg)
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
    self
  end

  def self.new(&blk)
    # ignores the block
    a = _alloc(0, nil)
    a.initialize(&blk)
  end

  def initialize(&blk)
    self
  end


  # Set intersection. Return new array containing elements common to two
  # arrays.
  # --
  # & uses Smalltalk implementation because Ruby Hash does not
  # support a removeKey:otherwise: in the Ruby API.
  primitive '_intersect',  'rubyIntersect:'

  def &(other)
    other = Type.coerce_to other, Array, :to_ary
    _intersect(other)
  end

  # Repetition: if +obj+ is a string, then <tt>arr.join(str)</tt>, otherwise
  # return a new array by concatenating +obj+ copies of self.
  def *(obj)
    if obj.respond_to? :to_str
      return join(obj)
    end

    val = Type.coerce_to(obj, Fixnum, :to_int)
    result = self.class.new # preserve subclass
    i = 0
    while i < val
      result.concat(self)
      i += 1
    end
    result
  end

  # Concatenation
  primitive '+', ','

  def -(arg)
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
      lim = size > other_size ? other_size : size # lim is the min
      i = 0
      while i < lim
        curr = self._at(i)
        if ts.include?(curr)
          unless cur.equal?(other[i])
            return 1 if size > other_size
            return -1 if size < other_size
          end
        else
          result = self._at(i) <=> other[i]
          return result if !(result.equal?(0))
        end
        i += 1
      end
      return size <=> other_size
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
    unless other._isArray
      return false unless other.respond_to?(:to_ary)
      other = other.to_ary
    end

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
        curr = self._at(i)
        if ts.include?(curr)
          return false unless curr.equal?(other[i])
        else
          return false unless curr == other[i]
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
    result = []
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

  primitive_nobridge 'concat', '_rubyAddAll:'
  primitive_nobridge 'concat*', '_rubyAddAll:'

  def delete(obj)
    n = 0
    lim = self.size
    while (n < lim)
      if obj == self._at(n)
        self.delete_at(n)
        return obj
      end
      n = n + 1
    end
    return nil
  end

  def delete(obj, &blk)
    n = 0
    lim = self.size
    while (n < lim)
      if obj == self._at(n)
        self.delete_at(n)
        return obj
      end
      n = n + 1
    end
    return blk.call
  end


  # Delete element at specified +index+.  Return the deleted item, or
  # +nil+ if no item at +index+
  primitive 'delete_at' , '_rubyDeleteAt:'

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
  end

  primitive 'empty?', 'isEmpty'

  # Return true if both are the same object, or if both are arrays, and
  # have the same number of elements and all corresponding elements are
  # eql?.
  #
  def eql?(other)
    return true if self.equal?(other)
    return false unless other._isArray
    return false unless size.equal?(other.size)

    ts = Thread._recursion_guard_set
    added = ts._add_if_absent(self)
    begin
      i = 0
      lim = size
      while i < lim
        curr = self._at(i)
        if ts.include?(curr)
          return false unless curr.equal?(other[i])
        else
          return false unless curr.eql?(other[i])
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

  def fetch(index)
    unless index._isFixnum
      index = index.to_int
    end
    my_siz = self.length
    if (index < 0)
      index = my_siz + index
    end
    if (index >= my_siz)
      raise IndexError
    end
    self._at(index)
  end

  def fetch(index, default)
    unless index._isFixnum
      index = index.to_int
    end
    my_siz = self.length
    if (index < 0)
      index = my_siz + index
    end
    if (index >= my_siz)
      return default
    end
    self._at(index)
  end

  def fetch(idx, &blk)
    index = idx
    unless index._isFixnum
      index = index.to_int
    end
    my_siz = self.length
    if (index < 0)
      index = my_siz + index
    end
    if (index >= my_siz)
      return blk.call(idx)
    end
    self._at(index)
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
    if count < 0
      raise ArgumentError, 'negative count'
    end
    self._at(0 , count)
  end

  def flatten
    flatten_onto([])
  end

  def flatten!
    replace(flatten)
  end

  # Note: The Pick Axe book has this method documented under both Array and
  # Enumerable.
  primitive 'include?', 'includes:'

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

  primitive '_last', 'last'
  def last(count = Undefined)
    # Smalltalk SequenceableCollection>>last raises exception calling last
    # on empty collection
    if self.size.equal?(0)
      return count.equal?(Undefined) ? nil : []
    end
    _last
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


  # replace written in Smalltalk
  # so it can use the copyFrom:to:into:startingAt  primitive
  primitive 'replace', 'rubyReplace:'

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

  def slice!(x, y = nil)
    if y
      return [] if size.equal?(0)
      result = self._at(x, y)
      self._at_put(x, y , [] )
    else
      result = self._at(x)
      unless result.equal?(nil)
        self._at_put(x,  nil )
      end
    end
    result
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

  primitive 'sort_by2&', 'sortBy:'
  def sort_by(&block)
    sort_by2{|a,b| block.call(a) <= block.call(b)}
  end

  def to_a
    self
  end

  def to_ary
    # TODO, if self.class is a subclass of Array , return
    #  an Array with elements of self ???
    self
  end

  def to_s
    self.join
  end

  # Transpose rows and columns (assumes self is an array of arrays, all of
  # the same length).  If self is not an array of Arrays, then we should
  # raise a TypeError trying to convert an element to an array.
  def transpose
    return [] if size.equal?(0)

    ary_size = self._at(0).size # we aren't empty
    i = 0
    lim = size
    # Check all contained arrays same length before we copy any data
    # TODO: Need to coerce to array...
    while i < lim
      if self._at(i).size != ary_size
        # TODO: can't raise a particular exception yet:
        #    raise IndexError "All contained arrays must be same length."
        raise "All contained arrays must be of same length."
      end
      i += 1
    end

    result = []
    i = 0
    while i < lim
      sub_ary = self._at(i)
      j = 0
      while j < ary_size
        # ||= doesn't yet work for array references...
        #   result[i] ||= []
        result[j] =  []  if result[j].equal?(nil)
        result[j][i] = sub_ary[j]
        j += 1
      end
      i += 1
    end
    result
  end

  def uniq
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
    ary
  end

  def uniq!
    old_size = size
    replace(uniq)
    return old_size.equal?(size) ? nil : self
  end

  # Prepend elements to self.  If no elements, return unmodified self.
  def unshift(*elements)
    insert_all(elements, 1)
    self
  end

  def values_at(*selectors)
    # selectors is an Array of
    lim = selectors.size
    n = 0
    res = []
    while (n < lim)
      idx = selectors[n]
      elem = self._at(idx)
      if (idx._isRange && ! elem.equal?(nil) )
    res.push(*elem)
      else
    res.push(elem)
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
