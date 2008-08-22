class Array
  # begin private helper methods
  # RxINC: Some of these don't begin with an '_'...
  primitive '_all', 'allSatisfy:'
  primitive '_detect', 'detect:ifNone:'
  primitive 'insert_all', 'insertAll:at:'
  primitive 'sort_by2&', 'sortBy:'
  primitive 'remove_first', 'removeFirst'
  primitive 'remove_if_absent', 'remove:ifAbsent:'
  primitive 'remove_last', 'removeLast'
  primitive '_fillFromToWith', 'fillFrom:to:with:'

  def flatten_onto(output)
    j = 0
    lim = size
    while (j < lim)
      el = self[j]
      el.flatten_onto(output)
      j = j + 1
    end
    output
  end
  # end private helper methods

  # Array Class Methods

  # Returns a new array with the given elements.
  def self.[](elements)
    raise "Method not implemented: Array.[]"
  end
  self.class.primitive 'alloc', 'new:'

  #
  def self.new(size=0, value=nil)
    inst = alloc(size)
    if value
      inst._fillFromToWith(0, size - 1 , value)
    end
    inst
  end

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
    j = 0
    while (j < argSize)
      el = arg[j]
      h[el] = el
      j = j + 1
    end
    j = 0
    while (j < mySize)
      el = self[j]
      if (h[el].equal?(default))
        res << el
      end
      j = j + 1
    end
    res
  end

  # note, <<  can't use smalltalk add: , it returns arg, not receiver
  primitive '<<', '_rubyAddLast:'

  # Comparison: return -1, 0, or 1.
  def <=>(other)
    raise "Method not implemented: Array#<=>"
  end
  primitive '==', '='

  primitive '[]' , '_rubyAt:'
  primitive '[]' , '_rubyAt:length:'

  primitive '[]=', '_rubyAt:put:'
  primitive '[]=', '_rubyAt:length:put:'

  # Set Union
  def |(other)
    raise "Method not implemented: Array#|"
  end

  # Associate: Search through self (an array of arrays).  Return first
  # array whose first element matches +key+ (using +key.==+).
  def assoc(key)
    raise "Method not implemented: Array#assoc"
  end

  primitive 'at' , '_rubyAt:'
  primitive 'clear', 'removeAll'

  def collect!(&b)
    i = 0
    while i < length
      self[i] = b.call(self[i])
      i += 1
    end
    self
  end

  # Return copy of self with all nil elements removed
  def compact
    raise "Method not implemented: Array#compact"
  end

  # Remove all nil elements from self
  def compact!
    raise "Method not implemented: Array#compact!"
  end

  primitive 'concat', 'addAll:'

  # TODO: need to add a block arg variant to delete
  def delete(el)
    remove_if_absent(el, proc{return nil})
    return el
  end

  # Delete element at specified +index+.  Return the deleted item, or
  # +nil+ if no item at +index+
  #   TODO: use the smalltalk primitive 
  def delete_at(index)
    raise "Method not implemented: Array#delete_at"
  end

  # Delete every element of self for which +block+ evalutes to +true+.
  def delete_if(&block)
    raise "Method not implemented: Array#delete_if"
  end

  def each(&b)
    i = 0
    while i < length
      b.call(self[i])
      i += 1
    end
    self
  end

  def each_index(&b)
    0.upto(size-1, &b)
  end

  primitive 'empty?', 'isEmpty'

  def eql?(other)
    raise "Method not implemented: Array#eql?"
  end

  def fetch(index,&b)
    raise "Method not implemented: Array#fetch"
  end
  def fill(obj)
    raise "Method not implemented: Array#fill"
  end
  primitive 'first'

  def flatten
    flatten_onto([])
  end

  def flatten!
    raise "Method not implemented: Array#flatten!"
  end

  # Note: The Pick Axe book has this method documented under both Array and
  # Enumerable.
  primitive 'include?', 'includes:'

  def index(el)
    i = 0
    sz = size
    while(i < sz)
      if(self[i] == el)
        return i
      end
      i += 1
    end
    nil
  end

  def indexes(*args)
    raise "Method not implemented: deprecated (use Array#values_at): Array#indexes"
  end
  def indicies(*args)
    raise "Method not implemented: deprecated (use Array#values_at): Array#indicies"
  end

  def insert(idx, *args)
    insert_all(args, idx+1)
  end

  def join(s="")
    out = ""
    max = length - 1
    for i in (0..max)
      out << self[i].to_s
      out << s unless i == max
    end
    out
  end

  primitive 'last'
  primitive 'length', 'size'

  alias map! collect!

  # Number of non-<tt>nil</tt> elements in self.
  def nitems
    raise "Method not implemented: Array#nitems"
  end
  primitive 'pack', 'rubyPack:'

  def pop
    unless empty?
      remove_last
    end
  end
  primitive 'push', '_rubyAddLast:'

  # Associate: Search through self (an array of arrays).  Return first
  # array whose second element matches +key+ (using +key.==+).
  def rassoc(key)
    raise "Method not implemented: Array#rassoc"
  end
  primitive 'reject!&', 'removeAllSuchThat:'

  # replace written in Smalltalk so it can use copyFrom:to:into:startingAt prim
  primitive 'replace', 'rubyReplace:'

  primitive 'reverse'

  def reverse!
    replace(reverse)
  end

  def reverse_each(&b)
    i = length - 1
    while(i >= 0)
      b.call(self[i])
      i -= 1
    end
  end

  def rindex(el)
    i = size - 1
    while(i >= 0)
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
  def sort
    d = dup
    d.sort!
    d
  end
  primitive 'sort!', 'sort'

  # RxINC: This implementation may be wrong.  See to_a vs to_ary in PickAxe.
  def to_a
    self
  end

  def to_ary
    self
  end

  def to_s
    self.join
  end

  # Transpose rows and columns (assumes self is an array of arrays)
  def transpose
    raise "Method not implemented: Array#transpose"
  end

  def uniq
    hash = {}
    ary = []
    j = 0
    lim = size
    while (j < lim)
      el = self[j]
      unless hash.include? el
        ary << el
        hash[el] = el
      end
      j = j + 1
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
    while i < length
      result[i] = b.call(self[i])
      i += 1
    end
    result
  end

  def detect(&block)
    _detect(block, nil)
  end

  def each_with_index(&block)
    raise "Method not implemented: Enumerable#each_with_index (Array.rb)"
  end

  alias entries to_a

  def find(&block)
    _detect(block, nil)
  end

  def find_all(&block)
    raise "Method not implemented: Enumerable#find_all (Array.rb)"
  end

  def grep(pattern)
    select{|ea| pattern === ea}
  end

  # include?: The Pick Axe book documens include? under both Array and
  # Enumerable. Our implementation is in the Array section above.

  primitive 'inject&', 'inject:into:'

  alias map collect

  def max
    raise "Method not implemented: Enumerable#max (Array.rb)"
  end

  primitive 'member?', 'includes:'

  def min
    raise "Method not implemented: Enumerable#min (Array.rb)"
  end

  def partition(&b)
    t = []
    f = []
    i = 0
    while i < length
      el = self[i]
      if(b.call(el))
        t << el
      else
        f << el
      end
    end
    [t,f]
  end

  def reject(&block)
    raise "Method not implemented: Enumerable#reject (Array.rb)"
  end

  primitive 'select&', 'select:'

  # sort: is listed in the Pick Axe book under both Array and Enumerable,
  # and implemented here in the Array section above.

  # PickAxe p 457 documents that sort_by uses Schwartzian Transform
  def sort_by(&block)
    sort_by2{|a,b| block.call(a) <= block.call(b)}
  end

  # to_a: Implemented above in the Array section

  def zip(*args)
    raise "Method not implemented: Enumerable#zip (Array.rb)"
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
