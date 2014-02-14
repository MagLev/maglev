# An +IdentitySet+ is a collection in which any distinct object can occur
# only once.  Adding the same (identical) object to an +IdentitySet+ multiple
# times is redundant.  The result is the same as adding it once.
#
# Since an +IdentitySet+ is an identity-based collection, different
# (non-identical) but equivalent (equal) objects are treated as distinct
# from each other.  In (equality) Sets, they are not distinct.  Adding
# multiple equivalent objects to an +IdentitySet+ yields an +IdentitySet+
# with as many elements as there are distinct equivalent objects.  In
# short, two different elements of an +IdentitySet+ are never identical,
# but they may be equivalent.
#
# You can create subclasses of +IdentitySet+ to restrict the kind of elements
# it contains.  When creating a subclass of +IdentitySet+, you must specify a
# class as the +constraint+ argument.  This class is called the element kind
# of the new subclass.  For each instance of the new subclass, the class of
# each element must be of the element kind.
#
# An +IdentitySet+ will never contain +nil+, i.e., <tt>set.add(nil)</tt>
# has no effect.
#
# IdentitySet is identically Smalltalk IdentitySet.
class IdentitySet

  class_primitive 'allocate', 'rubyBasicNew'

  def self.with_all(*array)
    o = self.allocate
    o.__addall(array)
    o
  end

  def self.with_all(arr)
    unless arr._isArray
      raise TypeError, 'expected an Array argument'
    end
    o = self.allocate
    o.__addall(arr)
    o
  end

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

  primitive_nobridge '__basic_dup', 'shallowCopy'  # uses singleton class for now
  primitive_nobridge '__basic_clone', 'shallowCopy' # use singleton class

  def dup
    res = self.__basic_dup
    res.initialize_copy(self)
    res
  end

  # clone inherited from Object

  primitive_nobridge '__removeAll', 'removeAll:'

  def avg(&block)
    self.sum(&block) / length
  end

  # Deletes the given object from the set and returns self.  Use +subtract+
  # to delete several items at once.
  def delete(obj)
    delete?(obj)
    self
  end

  # Remove all elements from reciever.  Returns self.
  def clear
    __removeAll(self)  # This takes an optimized path in the primitive
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
      s << "}>"
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

  #--
  # Psych/Yaml support.
  # IdentitySet also registered in lib/ruby/1.8/psych.rb
  #++

  # Psych hook method for dumping to YAML
  def encode_with(coder)
    # serialize as a YAML sequence
    coder.represent_seq(self.class.name, self.to_a)
  end

  # Psych hook method for reviving from YAML
  def init_with(coder)
    p coder.seq
    self.__addall(coder.seq)
  end

  #--
  # Add some of Enumerable's methods.  An Identity set is a
  # Non-sequenceable Collection, hence not all of Enumerable is applicable.
  #++

  def find(ifnone = nil)
    self.each { |o| return o if yield(o) }
    ifnone.call if ifnone
  end

  alias_method :detect, :find

  # call-seq:
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

  #--
  # ##################################################
  #          Index and Selection block support
  # ##################################################
  #++

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

  # call-seq:
  #   id_set.create_equality_index(path, class) => receiver
  #
  # Creates an equality index on the instance variable path specified by
  # +path+.  The equality index is ordered according to the comparison
  # operators provided by +class+ (which should be the class of the last
  # element in +path+).
  #
  # The +path+ describes the sequence of instance variable access that
  # leads to the instance variable to be indexed.  E.g., a path of '@foo',
  # will index on the <tt>@foo</tt> instance variable of the elements
  # directly held in the collection.  A path of '@foo.@bar' will index the
  # collection on the '@bar' instance variable of the object refered to by
  # the '@foo' instance variable of the elements in the collection.  The
  # path may be up to 16 levels deep.
  #
  # Currently, the instance variable that is indexed must be a fixed
  # instance variable (defined in the first opening of the class using the
  # __fixed_instvars directive).  We expect the indexing system to support
  #
  # A collection may have multiple indexes.
  #
  # Example:
  #   Create an index for a set of people.  The index is on the age field
  #   (a +Fixnum+) of the +Person+ class.  The objects are sorted using the
  #   comparison functions defined by the +Fixnum+ class (i.e., +age+ will
  #   be sorted numerically).
  #
  #     class Person
  #       attr_reader :name, :address, :age
  #       ...
  #     end
  #
  #     class Address
  #       attr_reader :street, :zip,...
  #       ...
  #     end
  #
  #     # People must contain only objects that have an @age instance
  #     # variable, and the value must be nil or a Fixnum
  #
  #     people = IdentitySet.new
  #     people.create_equality_index('@age', Fixnum)
  #     people << Person.new(...)
  #
  #   Create an additional index on the zipcode for all the people in the set:
  #
  #     people.create_equality_index('@address.@zip', Fixnum)
  #
  primitive_nobridge 'create_equality_index', 'createEqualityIndexOn:withLastElementClass:'

  # call-seq:
  #   id_set.create_identity_index(path) => receiver
  #
  # Creates an identity index on the path specified by the string.  An
  # identity index uses object identity for the comparison.  Generates an
  # error if +path+ is not a path for the element kind of the receiver or
  # if any term of the path except the last term is not constrained.
  #
  # See #create_equality_index for a description of +path+.
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
  #     # with a zip code.
  #
  #     employees = IdentitySet.new
  #     employees.create_identity_index('@address.@zipcode')
  #     employees << Employee.new("Fred", Address.new(...))
  #
  primitive_nobridge 'create_identity_index', 'createIdentityIndexOn:'

  primitive_nobridge '_search', 'select:comparing:with:'
  # Search the identity set for elements matching some criteria.  Makes use
  # of the index.  Assume +people+ is setup per comments for #create_index.
  # The following code will return an +IdentitySet+ (since +people+ is an
  # +IdentitySet+) with all of the +Person+ objects whose +age+ field is
  # less than 25.
  #
  #     youngsters = people.search([:@age], :<, 25)
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
  # find* etc.).  The operand path is converted to symbols before processing.
  def search(operand_path, query_op, query_val)
    operand_syms = operand_path.map { |p| p.to_sym }
    _search(operand_syms, st_op(query_op), query_val)
  end

  primitive_nobridge '_search_between', 'low:comparing:select:comparing:high:'
  # Search receiver for elements between +low_value+ and +high_value+.  By
  # default, the comparison operator for the low value is <tt>:lte</tt> and
  # the comparison operator for the high end is <tt>:lt</tt>.  E.g., to
  # search for people 18-25 (including 18 year olds, but excluding 25 year
  # olds):
  #
  #   results = people.search_between([:@age], 18, 25)
  #
  # If you wanted to exclude 18 year olds, and include 25 year olds, the
  # following will work:
  #
  #   results = people.search_between([:@age], 18, 25, :lt, :lte)
  #
  # The list of operands is listed in the #search doc.
  def search_between(operand_path, low_value, high_value, low_op=:lte, high_op=:lt)
    _search_between(low_value, st_op(low_op), operand_path, st_op(high_op), high_value)
  end

  # Search receiver for elements on +operand_path+ that fall within
  # +range+.  the comparison operator for the high end is
  # <tt>:lt</tt>.  E.g., to search for people 18-25 (including 18 year
  # olds, but excluding 25 year olds):
  #
  #   results = people.search_range([:@age], (18...25))
  #
  # If you wanted to exclude 18 year olds, and include 25 year olds, the
  # following will work:
  #
  #   results = people.search_range([:@age], (19..25))
  #
  # This method just wraps the appropriate call to <tt>search_between</tt>.
  def search_range(operand_path, range)
    high_op = range.exclude_end? ? QUERY_OPS[:lt] : QUERY_OPS[:lte]
    _search_between(range.begin, QUERY_OPS[:lte], operand_path, high_op, range.end)
  end

  # call-seq:
  #   id_set.remove_identity_index(path)
  #
  # If an identity index exists on +path+, and +path+ is not a proper
  # prefix of some indexed path, the the index is removed.  If the path
  # string is invalid or no index exists on the given path, an error is
  # raised.  If +path+ is an implicit index (due to the receiver's
  # participation as a set-valued instance variable in some other unordered
  # collection's index), then this method returns the path string."
  #
  # If an error occurs during index removal, it may not be possible to
  # commit the current transaction later.
  #
  # Indexes are not automatically destroyed when the collection they index
  # is destroyed, and must be removed explicitly.
  #
  #    people.create_identity_index('@age')
  #    ...
  #    people.remove_identity_index('@age')
  primitive_nobridge 'remove_identity_index', 'removeIdentityIndexOn:'

  # call-seq:
  #   id_set.remove_equality_index(path)
  #
  # If an equality index exists on +path+, remove that index.  If the path
  # string is invalid or no index exists on the given path, an error is
  # raised.  If +path+ is an implicit index (due to the receiver's
  # participation as a set-valued instance variable in some other unordered
  # collection's index), then this method returns the path string.
  #
  # If an error occurs during index removal, it may not be possible to
  # commit the current transaction later.
  #
  # Indexes are not automatically destroyed when the collection they index
  # is destroyed, and must be removed explicitly.
  #
  #    people.create_equality_index('@age', Fixnum)
  #    ...
  #    people.remove_equality_index('@age')
  primitive_nobridge 'remove_equality_index', 'removeEqualityIndexOn:'

  # call-seq:
  #   id_set.remove_all_indexes
  #
  # Remove all indexes from receiver.
  primitive_nobridge 'remove_all_indexes', 'removeAllIndexes'

  # call-seq:
  #   id_set.equality_indexed_paths => [path1, path2] of String
  #
  # Returns an array of strings, each of which represents a path for which
  # an equality index exists in the receiver.  Each path originates with
  # the elements of the receiver.
  primitive_nobridge 'equality_indexed_paths', 'equalityIndexedPaths'

  # call-seq:
  #   id_set.identity_indexed_paths => [path1, path2] of String
  #
  # Returns an array of strings, each of which represents a path for which
  # an identity index exists in the receiver.  Each path originates with
  # the elements of the receiver.
  primitive_nobridge 'identity_indexed_paths', 'identityIndexedPaths'

  # call-seq:
  #   id_set.kinds_of_index_on(path) => Symbol
  #
  # Returns a Symbol that indicates the kinds of indexes into the receiver
  # that exist on +path+.  Th: :identity, :equality, :equalityAndIdentity,
  # or :none (either 'path' is not a path for the element kind of the
  # receiver, or no indexes into the receiver exist on 'path').
  primitive_nobridge 'kinds_of_index_on', 'kindsOfIndexOn:'

  private
  # Return the appropriate Smalltalk version of +op+.  Raises an argument
  # error if +op+ is unrecognized.
  def st_op(op)
    st_op = QUERY_OPS[op]
    raise ArgumentError, "Unsupported query operation #{op.inspect}" if st_op.nil?
    st_op
  end
  #--
  # TODO: Expose the IndexManager so you can list un-referenced indexes...
  # TODO: createRc*IndexOn:
end
