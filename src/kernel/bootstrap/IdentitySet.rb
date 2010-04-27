class IdentitySet
  # Set is identically  Smalltalk IdentitySet
  #  an IdentitySet will never contain nil  ,  add(nil) will have no effect .

  # ------------------

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

  # ------------------

  # add(nil) will have no effect

  primitive_nobridge '<<', 'add:'
  primitive_nobridge 'add', 'add:'

  def add?(o)
    added = self.__add_if_absent(o)
    unless added
      return nil
    end
    self
  end

  primitive_nobridge '__add_if_absent', '_addIfAbsent:'

  # Removes anObject from the receiver and returns anObject. Returns nil if
  # anObject is missing from the receiver.
  primitive_nobridge 'delete?', 'removeIfPresent:'
  primitive_nobridge 'remove', 'removeIfPresent:'
  primitive_nobridge 'pop', 'removeIfPresent:'

  primitive_nobridge 'empty?', 'isEmpty'

  primitive_nobridge 'include?', 'includes:'
  primitive_nobridge 'member?', 'includes:'

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

  primitive 'length', 'size'
  primitive 'size', 'size'

  primitive_nobridge 'to_a' , 'asArray'

  primitive_nobridge '__basic_dup', '_basicCopy'  # uses singleton class for now
  primitive_nobridge '__basic_clone', '_basicCopy' # use singleton class
  # dup, clone inherited from Object

  # -------- following methods alphabetized
  #  see also Enumerable and indexing support sections below

  def avg(&block)
    self.sum(&block) / length
  end

  # Deletes the given object from the set and returns self.  Use +subtract+
  # to delete several items at once.
  def delete(obj)
    delete?(obj)
    self
  end

  # Deletes every element of the set for which block evaluates to
  # true, and returns self.
  def delete_if
    dup.each { |o| self.delete(o) if yield(o) }
    self
  end

  primitive_nobridge '__at', '_at:'  # one-based offset

  def each(&block)
    siz = self.size
    n = 1
    while n <= siz
      block.call( self.__at(n))
      n += 1
    end
    self
  end

  def group_by(&block)
    groups = {}
    self.each { |item|
      val = block.call(item)
      group = groups[val] ||= IdentitySet.new
      group << item
    }
    groups
  end

  # Return string with human readable representation of receiver.
  # E.g., "#<IdentitySet: {1, 2, 3}>".
  def inspect
    s = "#<IdentitySet: {"
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added
      s << '...}'
      return s
    end
    begin
      s << ( collect {|ea| ea.inspect }.join(", ") )
      s << "}"
    ensure
      ts.remove(self)
    end
    s
  end

  def proper_superset?(other)
    return false if size <= other.size
    self.superset?(other)
  end

  def proper_subset?(other)
    other.proper_superset?(self)
  end

  def subset?(other)
    other.superset?(self)
  end

  primitive_nobridge 'superset?', 'includesAllOf:'

  # Deletes every element that appears in the given enumerable object
  # and returns self.
  def subtract(enum)
    enum.each {|o| delete(o)}
    self
  end


  def sum(&block)
    s = 0
    self.each{|e| s += block.call(e)}
    s
  end

  # TODO
  #   clear, replace, flatten_merge, flatten, flatten!, collect!, reject!, merge, ^

  #--------------
  # Add some of Enumerable's methods.  An Identity set is a
  # Non-sequenceable Collection, hence not all of Enumerable
  # is applicable.
  #++


  def find(ifnone = nil)
    self.each { |o| return o if yield(o) }
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

  def count(item = MaglevUndefined, &block)
    seq = 0
    if item._equal?(MaglevUndefined)
      self.count(&block)
    else
      self.count(item)
    end
    seq
  end

  def count(item)
    seq = 0
    self.each { |o| seq += 1 if item == o }
    seq
  end

  def count(&block)
    seq = 0
    self.each { |o| seq += 1 if block.call(o) }
    seq
  end

  def find_all(&block)
    result = []
    self.each { |o| result << o if block.call(o) }
    result
  end
  alias_method :select, :find_all

  def reject(&block)
    result = []
    self.each { |o| result << o unless block.call(o) }
    result
  end

  def collect(&block)
    result = []
    if block_given?
      self.each { |o| result << yield(o) }
    else
      self.each { |o| result << o }
    end
    result
  end
  alias_method :map, :collect

  def inject(memo = MaglevUndefined, &block)
    uu = MaglevUndefined
    self.each { |o|
      if memo._equal?(uu)
        memo = o
      else
        memo = block.call(memo, o)
      end
    }
    memo._equal?(uu) ? nil : memo
  end


  def all?(&block)
    if block_given?
      self.each { |e| return false unless block.call(e) }
    else
      self.each { |e| return false unless e }
    end
    true
  end

  def any?(&block)
    if block_given?
      each { |o| return true if block.call(o) }
    else
      each { |o| return true if o }
    end
    false
  end

  # -------------
  # ##################################################
  #          Index and Selection block support
  # ##################################################

  # Ruby to smalltalk translation of query ops used in the indexed query
  # functions (search, search_between, etc.)
  QUERY_OPS = IdentityHash.from_hash( {
    :equal     => :==,
    :not_equal => :'~~',
    :eql       => :'=',
    :not_eql   => :'~=',
    :lt        => :<,
    :lte       => :<=,
    :gt        => :>,
    :gte       => :>= }  ).freeze

  # Creates an index on the path specified by the string.  The equality
  # index is ordered according to the sort-provided comparison operators
  # provided by the last element class.
  #
  # Example:
  #   Create an index for a set of people.  The index is on the age field
  #   (a Fixnum) of the Person class.  The objects are sorted using the
  #   comparison functions defined by the 'Fixnum' class (i.e., 'age' will
  #   be sorted numerically).
  #
  #     class Person
  #       attr_reader :name, :age  # Needs to be a fixed instance variable
  #       ...
  #     end
  #
  #     # People must contain only objects that have an @age instance
  #     # variable, and the value must be nil or a Fixnum
  #
  #     people = IdentitySet.new
  #     people.create_equality_index('age', Fixnum)
  #     people << Person.new(...)
  #
  # A collection may have multiple indexes.
  primitive_nobridge 'create_equality_index', 'createEqualityIndexOn:withLastElementClass:'


  # Creates an identity index on the path specified by the string.  An
  # identity index uses object identity for the comparison.  Generates an
  # error if +path+ is not a path for the element kind of the receiver or
  # if any term of the path except the last term is not constrained.
  #
  # If an error occurs during index creation, it may not be possible to
  # commit the current transaction later.
  #
  # Example:
  #     class Employee
  #       attr_reader :name, :address  # Needs to be a fixed instance variable
  #       ...
  #     end
  #
  #     class Address
  #        attr_reader :zipcode, ...
  #        ...
  #     end
  #
  #     # Employees must contain only objects that have an @address that
  #     # has a zip code.
  #
  #     employees = IdentitySet.new
  #     employees.create_identity_index('address.zipcode')
  #     employees << Employee.new("Fred", Address.new(...))
  #
  primitive_nobridge 'create_identity_index', 'createIdentityIndexOn:'

  primitive_nobridge '_search', 'select:comparing:with:'
  # Search the identity set for elements matching some criteria.  Makes
  # use of the index.  Assume people is setup per comments for
  # create_index.  The following code will return an IdentitySet (since
  # people is an IdentitySet) with all of the Person objects whose age
  # field is less than 25.
  #
  #     youngsters = people.search([:age], :<, 25)
  #
  # The supported comparison operations are
  # 1. <tt>:equal       # equal? (identical)</tt>
  # 2. <tt>:not_equal   # not equal? (not identical)</tt>
  # 3. <tt>:eql         # eql? (==)</tt>
  # 4. <tt>:not_eqll    # not eql? (!=)</tt>
  # 5. <tt>:lt          # less than</tt>
  # 6. <tt>:lte         # less than or equal</tt>
  # 7. <tt>:gt          # greater than</tt>
  # 8. <tt>:gte         # greater than or equal</tt>
  #
  # The name of this method was chosen so that it doesn't conflict with any
  # well known methods (e.g., Enumerable#select, Enumerable#find*, Rails
  # find* etc.).
  def search(operand_path, query_op, query_val)
    st_op = QUERY_OPS[query_op]
    raise ArgumentError, "Unsupported query_op #{query_op.inspect}" if st_op.nil?
    _search(operand_path, st_op, query_val)
  end

  primitive_nobridge '_search_between', 'low:comparing:select:comparing:high:'
  # Search receiver for elements between +low_value+ and +high_value+.  By
  # default, the comparison operator for the low value is <tt>:lte</tt> and
  # the comparison operator for the high end is <tt>:lt</tt>.  E.g., to
  # search for people 18-25 (including 18 year olds, but excluding 25 year
  # olds):
  #
  #   results = people.search_between([:age], 18, 25)
  #
  # If you wanted to exclude 18 year olds, and include 25 year olds, the
  # following will work:
  #
  #   results = people.search_between([:age], 18, 25, :lt, :lte)
  #
  # The list of operands is listed in the #search doc.
  def search_between(operand_path, low_value, high_value, low_op=:lte, high_op=:lt)
    _search_between(low_value, QUERY_OPS[low_op], operand_path, QUERY_OPS[high_op], high_value)
  end

  # Search receiver for elements on +operand_path+ that fall within
  # +range+.  the comparison operator for the high end is
  # <tt>:lt</tt>.  E.g., to search for people 18-25 (including 18 year
  # olds, but excluding 25 year olds):
  #
  #   results = people.search_between([:age], (18...25))
  #
  # If you wanted to exclude 18 year olds, and include 25 year olds, the
  # following will work:
  #
  #   results = people.search_between([:age], (19..25))
  #
  # This method just wraps the appropriate call to
  # <tt>search_between</tt>.
  def search_range(operand_path, range)
    high_op = range.exclude_end? ? QUERY_OPS[:lt] : QUERY_OPS[:lte]
    _search_between(range.begin, QUERY_OPS[:lte], operand_path, high_op, range.end)
  end

  # Remove the specified index from receiver.
  #
  # Indexes will stay around, even if the collection they index is
  # destroyed (think running your test cases, over and over and not
  # cleaning up the indexes...). .
  #
  #    people.remove_index('age')
  primitive_nobridge 'remove_identity_index', 'removeIdentityIndexOn:'

  primitive_nobridge 'remove_equality_index', 'removeEqualityIndexOn:'

  # Remove all indexes from the receiver
  primitive_nobridge 'remove_all_indexes', 'removeAllIndexes'

  # Returns an Array of Strings, each of which represents a path for which
  # an equality index exists in the receiver.  Each path originates with
  # the elements of the receiver.
  primitive_nobridge 'equality_indexed_paths', 'equalityIndexedPaths'

  # Returns an Array of Strings, each of which represents a path for which
  # an identity index exists in the receiver.  Each path originates with
  # the elements of the receiver.
  primitive_nobridge 'identity_indexed_paths', 'identityIndexedPaths'

  # Returns a Symbol that indicates the kinds of indexes into the receiver
  # that exist on 'path': :identity, :equality, :equalityAndIdentity,
  # or :none (either 'path' is not a path for the element kind of the
  # receiver, or no indexes into the receiver exist on 'path').
  primitive_nobridge 'kinds_of_index_on', 'kindsOfIndexOn:'

  # TODO: Expose the IndexManager so you can list un-referenced indexes...
  # TODO: createRc*IndexOn:
end
