class IdentitySet
   # Set is identically  Smalltalk IdentitySet
   #  an IdentitySet will never contain nil  ,  add(nil) will have no effect .

    primitive_nobridge '<<', 'add:'
    primitive_nobridge 'add', 'add:'
    primitive_nobridge '_add_if_absent', '_addIfAbsent:'
    primitive_nobridge '*'
    primitive_nobridge '+'
    primitive_nobridge '-'
    primitive_nobridge '==' , '='
    primitive '_addall', 'addAll:'
    primitive 'each&', 'do:'
    primitive 'length', 'size'

    primitive_nobridge 'include?', 'includes:'
 
    primitive_nobridge 'remove', 'removeIfPresent:'
    primitive_nobridge 'pop', 'removeIfPresent:'
    # returns argument, or nil if object was not present

    primitive_nobridge 'delete?', 'removeIfPresent:'

    primitive_nobridge '_basic_dup', '_basicCopy'  # uses singleton class for now
    primitive_nobridge '_basic_clone', '_basicCopy' # use singleton class
    # dup, clone inherited from Object

    def delete(obj)
      delete?(obj)
      self
    end

    class_primitive 'new' , 'new'

    def self.with_all(*array)
      o = new
      o._addall(*array)
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
    #     my_peeps._create_index('age', Fixnum)
    #
    # A collection may have multiple indexes.
    primitive_nobridge '_create_index', 'createEqualityIndexOn:withLastElementClass:'

    # Search the identity set for elements matching some criteria.  Makes
    # use of the index.  Assume my_peeps is setup per comments for
    # _create_index.  The following code will return an IdentitySet (since
    # my_peeps is an IdentitySet) with all of the Person objects whose age
    # field is less than 25.
    #
    #     youngsters = my_peeps._select([:age], :<, 25)
    #
    # The following comparsison operators are allowed: TBD...
    primitive_nobridge '_select', 'select:comparing:with:'

    # Remove an the specified index from receiver.
    #
    # Indexes will stay around, even if the collection they index is
    # destroyed (think running your test cases, over and over and not
    # cleaning up the indexes...). .
    #
    #    my_peeps._remove_index('age')
    primitive_nobridge '_remove_index', 'removeIdentityIndexOn:'

    # Remove all indexes from the receiver
    primitive_nobridge '_remove_all_indexes', 'removeAllIndexes'

    # TODO: Expose the IndexManager so you can list un-referenced indexes...
end
