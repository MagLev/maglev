class Array
  Undefined = Object.new  # TODO: Make one Undefined object in the system

  # begin private helper methods
  # TODO: Some of these don't begin with an '_'...
  primitive_nobridge '_all', 'allSatisfy:'
  primitive_nobridge '_detect', 'detect:ifNone:'
  primitive_nobridge '_fillFromToWith', 'fillFrom:to:with:'
  primitive_nobridge 'insert_all', 'insertAll:at:'
  primitive_nobridge 'remove_first', 'removeFirst'
  primitive_nobridge 'remove_if_absent', 'remove:ifAbsent:'
  primitive_nobridge 'remove_last', 'removeLast'

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

  def self.new(size=0, value=nil)
    _alloc(size, value)
  end

  #  Array.new(aSize) {|i| block| } # TODO form not supported yet

  # Array Instance methods

  # Set intersection. Return new array containing elements common to two
  # arrays.
  # --
  # & uses Smalltalk implementation because Ruby Hash does not
  # support a removeKey:otherwise: in the Ruby API.
  primitive '&',  'rubyIntersect:'

  # Repetition: if +obj+ is a string, then <tt>arr.join(str)</tt>, otherwise
  # return a new array by concatenating +obj+ copies of self.
  def *(obj)
    result = []
    # TODO:  not checking for  obj responds to to_str
    # TODO: do not use a block here.
    obj.times{result.concat(self)}
    result
  end

  # Concatenation
  primitive '+', ','

  def -(arg)
    argSize = arg.size
    mySize = size
    default = Array.new
    h = Hash.new(default)
    res = Array.new
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
    unless other._isArray
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

  primitive 'concat', '_rubyAddAll:'

  # TODO: need to add a block arg variant to delete
  def delete(el)
    remove_if_absent(el, proc{return nil})
    return el
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
    self.size= fill_idx
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

  def fetch(index,&b)
    # TODO: Allen will do, maybe use _rubyAt: primitive variant
    raise "Method not implemented: Array#fetch"
  end

  def fill(obj, start=nil, length=nil)
    # TODO: Needs block support
    start  ||= 0
    sz = size
    length ||= sz
    if (start < 0)
      start = sz + start
      if (start < 0)
        start = 0
      end
    end
    if (length < 0)
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

  def first
    self[0]
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
  end

  def join(s="")
    out = ""
    max = length - 1
    i = 0
    while i < max
      out << self[i].to_s
      out << s
      i = i + 1
    end
    out << self[max].to_s
    out
  end

  primitive '_last', 'last'
  def last(count = Undefined)
    # Smalltalk SequenceableCollection>>last raises exception calling last
    # on empty collection
    if self.empty?
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

  def pop
    unless empty?
      remove_last
    end
  end

  primitive 'push*', '_rubyAddArguments:'

  # Associate: Search through self (an array of arrays).  Return first
  # array whose second element matches +key+ (using +key.==+).
  def rassoc(key)
    _assoc(key, 1)
  end


  primitive 'reject!&', 'removeAllSuchThat:'

  # replace written in Smalltalk so it can use copyFrom:to:into:startingAt
  # prim
  primitive 'replace', 'rubyReplace:'

  primitive 'reverse'

  def reverse!
    replace(reverse)
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

  def shift
    unless empty?
      remove_first
    end
  end

  primitive 'size'
  primitive 'slice', '_rubyAt:'
  primitive 'slice', '_rubyAt:length:'

  def slice!(x, y = nil)
    if y
      result = self[x,y]
      self[x,y] = nil
    else
      result = self[x]
      self[x] = nil
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
    return [] if empty?

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
    replace(uniq)
  end

  def unshift(element)
    insert_all([element], 1)
    self
  end

  # Return an array containing the element in self at the positions given
  # by the selectors.
  def values_at(*selectors)
    # iteration with while loop, using  [] primitive and appending to result
    #   use either  self.[idx,1] or  self.[aRange]
    #   use << to append to result
    raise "Method not implemented: Array#values_at"
  end

  # ========== Enumerable ========================
  #
  # Implementation of Enumerable.  These methods are defined here
  # instead of in Enumerable for a couple of reasons:
  # (A) Enumerable was not yet a module
  # (B) Mixins were not yet implemented
  # (C) Some of the methods are performance sensitive.
  #
  # After Enumerable is implemented, we should re-visit these and ensure
  # only the methods critical for performane or Array specific
  # implementations are implemented here.

  def all?(&b)
    _all(b)
  end

  primitive 'any?&', 'anySatisfy:'

  def collect(&b)
    result = Array.new(length)
    i = 0
    lim = size
    while i < lim
      result[i] = b.call(self[i])
      i += 1
    end
    result
  end

  def detect(&block)
    _detect(block, nil)
  end

  def each_with_index(&block)
    i = 0
    lim = size
    while i < lim
      block.call(self[i],i)
      i += 1
    end
  end

  alias entries to_a

  def find(&block)
    _detect(block, nil)
  end

  def find_all(&block)
    result = []
    i = 0
    lim = size
    while i < lim
      el = self[i]
      result << el if block.call(el)
      i += 1
    end
    result
  end

  def grep(pattern)
    select{|ea| pattern === ea}
  end

  # include?: The Pick Axe book documens include? under both Array and
  # Enumerable. Our implementation is in the Array section above.

  primitive 'inject&', 'inject:into:'

  alias map collect

  def max
    if size.equal?(0)
      nil
    else
      max_v = self[0]
      i = 1
      lim = size
      while i < lim
        max_v = self[i] if (self[i] <=> max_v) > 0
        i += 1
      end
      max_v
    end
  end

  primitive 'member?', 'includes:'

  def min
    if size.equal?(0)
      nil
    else
      min_v = self[0]
      i = 1
      lim = size
      while i < lim
        min_v = self[i] if (self[i] <=> min_v) < 0
        i += 1
      end
      min_v
    end
  end

  def partition(&b)
    t = []
    f = []
    i = 0
    lim = size
    while i < lim
      el = self[i]
      if(b.call(el))
        t << el
      else
        f << el
      end
      i += 1
    end
    [t,f]
  end

  def reject(&b)
    result = []
    i = 0
    lim = size
    while i < lim
      result << self[i] unless b.call(self[i])
      i += 1
    end
    result
  end

  primitive 'select&', 'select:'

  # sort: is listed in the Pick Axe book under both Array and Enumerable,
  # and implemented here in the Array section above.

  # PickAxe p 457 documents that sort_by uses Schwartzian Transform
  # sort_by  implemented in Array section above

  # to_a: Implemented above in the Array section

  def zip(*args)
    result = []
    args = args.map { |a| a.to_a }  # TODO: loop-ize
    i = 0
    lim = size
    while i < lim
      ary = [self[i]]

      j = 0
      while j < args.length
        ary << args[j][i]
        j += 1
      end
      #  b.call(ary)...
#       # yield(ary) if block_given? # TODO: Uncomment when proper
#                                    # block_given? is implemented
      result << ary
      i += 1
    end
    result
  end

  # Overrides from Object that are not documented in Array (e.g., eql? is
  # documented in Array, so is not listed here).
  primitive 'clone', 'copy'
  primitive 'dup', 'copy'
  primitive 'hash'
#  primitive 'inspect', 'printString'
  def inspect
    "[" + collect{|ea| ea.inspect}.join(", ") + "]"
  end

end

def Array(arg)
  arg.to_a rescue [arg]
end
