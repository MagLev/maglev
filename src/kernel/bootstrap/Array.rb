class Array
  # begin private helper methods
  # TODO: Some of these don't begin with an '_'...
  primitive_nobridge '_fillFromToWith', 'fillFrom:to:with:'
  primitive_nobridge 'insert_all', 'insertAll:at:'

  primitive 'size=', 'size:'
  class_primitive_nobridge '_withAll', 'withAll:'

  # TODO consider a method prefix __nobr__ which during load prims
  #   would suppress generation of bridge methods for private
  #   methods that should not need bridge methods

  def _anySatisfyCaseLeaf( obj )
    # used in implementation of  while *list   within a   case
    n = 0
    lim = size
    while n < lim
      el = self[n]
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
      el = self[n]
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
      el = self[i]
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
      el = self[i]
      el.flatten_onto(output)
      i = i + 1
    end
    output
  end
  # end private helper methods

  # Array Class Methods

  # Returns a new array with the given elements.
  def self.[](*elements)
    _withAll(elements)
  end
  class_primitive_nobridge '_alloc', '_rubyNew:initValue:'

  def self.new(a_size=0, value=nil)
    s = Type.coerce_to(a_size, Integer, :to_int)
    _alloc(s, value)
  end

  def self.new(arg)
    # this method will have no bridge methods, all the bridges
    #  will map to the previous 2 arg form
    if (arg._isFixnum)
      new(arg, nil)
    else
      _coerceOneArg(arg)
    end
  end

  def self._coerceOneArg(arg)
    # separate method to avoid complex blocks in self.new(arg)
    begin
      ary = Type.coerce_to(arg, Array, :to_ary)
      res = _withAll(ary)
    rescue TypeError
      siz = Type.coerce_to(arg, Fixnum, :to_int)
      res = _alloc(siz, nil)
    end
    res
  end


  def self.new(size, &blk)
    # will have no bridge methods
    unless size._isFixnum
      raise ArgumentError, 'size is not a Fixnum'
    end
    a = new(size, nil)
    n = 0
    while (n < size)
      a[n] = blk.call(n)
      n = n + 1
    end
    a
  end

  # Array Instance methods

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
    result = []
    if obj._isFixnum
      for i in 0..obj-1
        result.concat(self)
      end
    else
      obj.times { |i|
        result.concat(self)
      }
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
      el = self[i]
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

    i = 0
    unless other._isArray
      raise TypeError , "not an Array"
    end

    lim = size > other.size ? other.size : size # lim is the min
    while i < lim
      result = self[i] <=> other[i]
      return result if result != 0
      i += 1
    end
    size <=> other.size
  end

  # ====== Comparable:
  # RxINC: This is a cut-n-paste to get things working for mspec.
  # Need to either overwrite or allow a mixin.

  def ==(other)
    if other._isArray
      if (other.equal?(self))
        return true
      end
    else
      return false
    end
    lim = size
    unless lim.equal?(other.size)
      return false
    end
    i = 0
    while i < lim
      unless self[i] == other[i]
        return false
      end
      i += 1
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

  primitive_nobridge '[]=', '_rubyAt:put:'
  primitive_nobridge '[]=', '_rubyAt:length:put:'

  # Set Union.  Removes duplicates from self: [1,1,1] | [] => [1]
  def |(other)
    hash = {}
    ary = []
    i = 0
    lim = size
    while i < lim
      el = self[i]
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
      self[i] = b.call(self[i])
      i += 1
    end
    self
  end

  # Return copy of self with all nil elements removed
  def compact
    result = []
    i = 0
    lim = size
    while i < lim
      el = self[i]
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
      break if self[i].equal?(nil)
      i += 1
    end
    return nil if i.equal?(lim)

    fill_idx = i
    while i < lim
      el = self[i]
      unless el.equal?(nil)
        self[fill_idx] = el
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
      if obj == self[n]
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
      if obj == self[n]
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
      break if block.call(self[i])
      i += 1
    end

    fill_idx = i
    while i < lim
      el = self[i]
      unless block.call(el)
        self[fill_idx] = el
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
      b.call(self[i])
      i += 1
    end
    self
  end

  def each_with_index(&b)
    i = 0
    lim = size
    while i < lim
      b.call(self[i], i)
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
    return true if equal? other
    return false unless other._isArray
    return false unless size.equal?(other.size)

    i = 0
    lim = size
    while i < lim
      return false unless self[i].eql? other[i]
      i += 1
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
    self[index]
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
    self[index]
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
    self[index]
  end

  #  note multiple variants below
  def fill(obj, start=nil, length=nil)
    unless start._isFixnum
      if start.equal?(nil)
        start = 0
      else
        start = start.to_int
      end
    end
    unless length._isFixnum
      unless length.equal?(nil)
        length = length.to_int
      end
    end
    sz = size
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
      start.each do | n |
        self.fill(obj, n, 1)
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
      self[n] = blk.call(n)
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
      start.each do | n |
        fill(start, 1, &lk)
      end
    else
      fill(start, nil, blk)
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
    self[0]
  end

  def first(count)
    if count < 0
      raise ArgumentError, 'negative count'
    end
    self[0,count]
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
      if(self[i] == el)
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

  def insert(idx, *args)
    insert_all(args, idx+1)
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
      count += 1 unless self[i].equal?(nil)
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
      elem = self[idx]
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
      a = self[low]
      b = self[high]
      self[high] = a
      self[low]   = b
      low = low + 1
      high = high - 1
    end
    self
  end

  def reverse_each(&b)
    i = length - 1
    while i >= 0
      b.call(self[i])
      i -= 1
    end
  end

  def rindex(el)
    i = size - 1
    while i >= 0
      if(self[i] == el)
        return i
      end
      i -= 1
    end
    nil
  end

  primitive_nobridge '_remove_from_to_', 'removeFrom:to:'

  def shift
    sz = self.size
    unless sz.equal?(0)
      elem = self[0]
      _remove_from_to_(1, 1)
      elem
    end
  end

  primitive 'size'
  primitive 'slice', '_rubyAt:'
  primitive 'slice', '_rubyAt:length:'

  def slice!(x, y = nil)
    if y
      return [] if size == 0
      result = self[x,y]
      self[x,y] = []
    else
      result = self[x]
      unless result.nil?
        self.delete_at(x)
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
      _sort!{ | a, b| a <= b }
    end
  end

  def sort(&blk)
    d = dup
    if (block_given?)
      d._sort!{ | a, b| blk.call(a, b) <= 0 }
    else
      d._sort!{ | a, b| a <= b }
    end
    d
  end

  primitive 'sort_by2&', 'sortBy:'
  def sort_by(&block)
    sort_by2{|a,b| block.call(a) <= block.call(b)}
  end

  # TODO: This implementation may be wrong.  See to_a vs to_ary in PickAxe.
  def to_a
    self
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
    return [] if size.equal?(0)

    ary_size = self[0].size # we aren't empty
    i = 0
    lim = size
    # Check all contained arrays same length before we copy any data
    # TODO: Need to coerce to array...
    while i < lim
      if self[i].size != ary_size
        # TODO: can't raise a particular exception yet:
        #    raise IndexError "All contained arrays must be same length."
        raise "All contained arrays must be of same length."
      end
      i += 1
    end

    result = []
    i = 0
    while i < lim
      sub_ary = self[i]
      j = 0
      while j < ary_size
        # ||= doesn't yet work for array references...
        #   result[i] ||= []
        result[j] = [] if result[j].equal?(nil)
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
      el = self[i]
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
      elem = self[idx]
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

  primitive 'hash'

  def inspect(touchedSet=nil)
    s = "["
    if (touchedSet.equal?(nil))
      touchedSet = IdentitySet.new
    else
      if (touchedSet._includes(self))
        s << '...]'
        return s
      end
    end
    touchedSet << self
    s << ( collect{|ea| ea.inspect(touchedSet) }.join(", ") )
    s << "]"
    s
  end

end

def Array(arg)
  arg.to_a rescue [arg]
end
