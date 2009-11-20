class IdentitySet
  # Set is identically  Smalltalk IdentitySet
  #  an IdentitySet will never contain nil  ,  add(nil) will have no effect .

  primitive_nobridge '<<', 'add:'
  primitive_nobridge 'add', 'add:'
  primitive_nobridge '__add_if_absent', '_addIfAbsent:'
  primitive_nobridge '*'
  primitive_nobridge '+'
  primitive_nobridge '-'
  primitive_nobridge '==' , '='
  primitive '__addall', 'addAll:'
  primitive 'each&', 'do:'
  primitive 'length', 'size'
  primitive 'size', 'size'

  primitive_nobridge 'include?', 'includes:'

  primitive_nobridge 'remove', 'removeIfPresent:'
  primitive_nobridge 'pop', 'removeIfPresent:'
  # returns argument, or nil if object was not present

  primitive_nobridge 'delete?', 'removeIfPresent:'

  primitive_nobridge '__basic_dup', '_basicCopy'  # uses singleton class for now
  primitive_nobridge '__basic_clone', '_basicCopy' # use singleton class
  # dup, clone inherited from Object

  # primitive_nobridge '__detect', 'detect:' # not used yet

  primitive_nobridge 'to_a' , 'asArray'

  def delete(obj)
    delete?(obj)
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
    unless item.equal? Undefined
      each { |o| seq += 1 if item == o }
    else
      each { |o| seq += 1 if yield(o) }
    end
    seq
  end

  primitive_nobridge 'select', 'select:'
  alias_method :find_all, :select

  primitive_nobridge 'reject', 'reject:'
  primitive_nobridge 'collect', 'collect:'
  alias_method :map, :collect

  primitive_nobridge 'inject', 'inject:into:'
  primitive_nobridge 'all?', 'allSatisfy:'
  primitive_nobridge 'any?', 'anySatisfy:'

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
  #     youngsters = my_peeps.select([:age], :<, 25)
  #
  # The following comparsison operators are allowed: TBD...
  primitive_nobridge 'select', 'select:comparing:with:'

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
