class IdentitySet
  # Set is identically  Smalltalk IdentitySet
  #  an IdentitySet will never contain nil  ,  add(nil) will have no effect .

  primitive_nobridge '<<', 'add:'
  primitive_nobridge 'add', 'add:'
  def add?(o)
    self.include?(o) ? nil : self.add(o)
  end

  primitive_nobridge '__add_if_absent', '_addIfAbsent:'

  primitive_nobridge '&', '*'
  primitive_nobridge 'intersection', '*'

  # Returns a new set built by merging the set and the elements of the
  # given enumerable object.
  primitive_nobridge '+'
  primitive_nobridge '|', '+'
  primitive_nobridge 'union', '+'

  primitive_nobridge '-'
  primitive_nobridge 'difference', '-'

  primitive_nobridge '==' , '='
  primitive '__addall', 'addAll:'
  primitive 'each&', 'do:'
  primitive 'length', 'size'
  primitive 'size', 'size'

  primitive_nobridge 'include?', 'includes:'
  primitive_nobridge 'member?', 'includes:'
  primitive_nobridge 'superset?', 'includesAllOf:'
  def proper_superset?(other)
    return false if size <= other.size
    self.superset?(other)
  end

  def subset?(other)
    other.superset?(self)
  end

  def proper_subset?(other)
    other.proper_superset?(self)
  end

  # Removes anObject from the receiver and returns anObject. Returns nil if
  # anObject is missing from the receiver.
  primitive_nobridge 'delete?', 'removeIfPresent:'
  primitive_nobridge 'remove', 'removeIfPresent:'

  # Deletes the given object from the set and returns self.  Use +subtract+
  # to delete several items at once.
  def delete(o)
    delete?(o)
    self
  end

  # Deletes every element of the set for which block evaluates to
  # true, and returns self.
  def delete_if
    dup.each { |o| self.delete(o) if yield(o) }
    self
  end

  primitive_nobridge 'pop', 'removeIfPresent:'
  # returns argument, or nil if object was not present

  primitive_nobridge '__basic_dup', '_basicCopy'  # uses singleton class for now
  primitive_nobridge '__basic_clone', '_basicCopy' # use singleton class
  # dup, clone inherited from Object

  # primitive_nobridge '__detect', 'detect:' # not used yet

  primitive_nobridge 'to_a' , 'asArray'
  primitive_nobridge 'empty?', 'isEmpty'

  # TODO
  #   clear, replace, flatten_merge, flatten, flatten!, collect!, reject!, merge, ^

  def delete(obj)
    delete?(obj)
    self
  end

  # Deletes every element that appears in the given enumerable object
  # and returns self.
  def subtract(enum)
    enum.each {|o| delete(o)}
    self
  end

  class_primitive 'new' , 'new'

  def self.with_all(*array)
    o = self.new
    o.__addall(array)
    o
  end

  def self.with_all(arr)
    unless arr._isArray
      raise TypeError, 'expected an Array argument'
    end
    o = self.new
    o.__addall(arr)
    o
  end

  def inspect
    "[[#{length}]]"
  end

  def group_by(&block)
    groups = {}
    each do |item|
      val = block.call(item)
      group = groups[val] ||= IdentitySet.new
      group << item
    end
    groups
  end

  def sum(&block)
    s = 0
    each{|e| s += block.call(e)}
    s
  end

  def avg(&block)
    sum(&block) / length
  end

  # ##################################################
  # Add some of Enumerable's methods.  An Identity set is a
  # Non-sequenceable Collection, hence not all of Enumerable
  # is applicable.
  # ##################################################
  def find(ifnone = nil)
    each { |o| return o if yield(o) }
    ifnone.call if ifnone
  end

  alias_method :detect, :find

  #   enum.count(item)             => int
  #   enum.count { | obj | block } => int
  #
  # Returns the number of items in +enum+ for which equals to +item+. If a
  # block is given, counts the number of elements yielding a true value.
  #
  #   ary = [1, 2, 4, 2]
  #   ary.count(2)          # => 2
  #   ary.count{ |x|x%2==0}  # => 3

  def count(item = Undefined)
    seq = 0
    unless item._equal?(Undefined)
      each { |o| seq += 1 if item == o }
    else
      each { |o| seq += 1 if yield(o) }
    end
    seq
  end

  def find_all
    result = Array.new
    each { |o| result << o if yield(o) }
    result
  end
  alias_method :select, :find_all

  def reject
    result = Array.new
    each { |o| result << o unless yield(o) }
    result
  end

  def collect
    result = Array.new
    if block_given?
      each { |o| result << yield(o) }
    else
      each { |o| result << o }
    end
    result
  end
  alias_method :map, :collect

  def inject(memo = Undefined)
    each { |o|
      if memo._equal? Undefined
        memo = o
      else
        memo = yield(memo, o)
      end
    }
    memo._equal?(Undefined) ? nil : memo
  end

  def all?
    if block_given?
      each { |e| return false unless yield(e) }
    else
      each { |e| return false unless e }
    end
    true
  end

  def any?(&prc)
    prc = Proc.new { |obj| obj } unless block_given?
    each { |o| return true if prc.call(o) }
    false
  end

  # ##################################################
  #                  EXPERIMENTAL:
  # ##################################################
  #
  # experimental support for the indicies and "selection blocks" Still to
  # be done: Need to expose Rc* (reduced conflict) versions for high
  # concurrency scenarios.  The current API is probably good enough to
  # get feedback on the direction we should take...

  # Creates an index on the path specified by the string.  The equality
  # index is ordered according to the sort-provided comparison operators
  # provided by the last element class.
  #
  # Example:
  #   Create an index for a set of people.  The index is on the age field
  #   (a Fixnum) of the Person class:
  #
  #     class Person
  #       attr_reader :name, :age  # probably need fixed instVars here
  #       ...
  #     end
  #
  #     my_peeps = IdentitySet.new   # Will contain only Person objects
  #     my_peeps.create_index('age', Fixnum)
  #
  # A collection may have multiple indexes.
  primitive_nobridge 'create_index', 'createEqualityIndexOn:withLastElementClass:'

  # Search the identity set for elements matching some criteria.  Makes
  # use of the index.  Assume my_peeps is setup per comments for
  # create_index.  The following code will return an IdentitySet (since
  # my_peeps is an IdentitySet) with all of the Person objects whose age
  # field is less than 25.
  #
  #     youngsters = my_peeps.search([:age], :<, 25)
  #
  # The name of this method was chosen so that it doesn't conflict with any
  # well known methods (e.g., Enumerable#select, Enumerable#find*, Rails
  # find* etc.).
  primitive_nobridge 'search', 'select:comparing:with:'

  # Remove an the specified index from receiver.
  #
  # Indexes will stay around, even if the collection they index is
  # destroyed (think running your test cases, over and over and not
  # cleaning up the indexes...). .
  #
  #    my_peeps.remove_index('age')
  primitive_nobridge 'remove_index', 'removeIdentityIndexOn:'

  # Remove all indexes from the receiver
  primitive_nobridge 'remove_all_indexes', 'removeAllIndexes'

  # TODO: Expose the IndexManager so you can list un-referenced indexes...
end
