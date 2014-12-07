
# RUBINIUS: This file taken from Rubinius, and modified.

# depends on: module.rb class.rb

##
#  The Enumerable mixin provides collection classes with  several traversal
#  and searching methods, and with the ability to sort. The class must provide
#  a method #each, which yields successive members of the collection. If
#  Enumerable#max, #min, or #sort is used, the objects in the collection must
#  also implement a meaningful <tt><=></tt> operator, as these methods rely on
#  an ordering between members of the collection.

module Enumerable

  ##
  #--
  # Just to save you 10 seconds, the reason we always use #each to extract
  # elements instead of something simpler is because Enumerable can not assume
  # any other methods than #each. If needed, class-specific versions of any of
  # these methods can be written *in those classes* to override these.

  class Sort  # [

    def initialize(sorter = nil)
      @sorter = sorter
    end

    def sort(xs, &prc)
      # The ary should be inmutable while sorting
      # prc = Proc.new { |a,b| a <=> b } unless block_given?

      if @sorter
        @sorter = method(@sorter) unless @sorter.respond_to?(:call)
        @sorter.call(xs, &prc)
      else
        # quicksort(xs, &prc)
        #    use Smalltalk mergesort from Array
        if block_given?
          mergesort(xs) { | a, b|
            c = prc.call(a, b)
            if c._equal?(nil)
              raise ArgumentError, 'not comparable'
            end
            c <= 0
          }
        else
          mergesort(xs) { |a, b|
            c = (a <=> b)
            if c._equal?(nil)
              raise ArgumentError, 'not comparable'
            end
            c <= 0
          }
        end
      end
    end

    alias_method :call, :sort

    def mergesort(xs, &block)
      arr = []
      xs.each { | o | arr << o }
      arr.__sort!(&block)  # sorts array in place
    end

    ##
    # Sort an Enumerable using simple quicksort (not optimized)

    def quicksort(xs, &prc)
      return [] unless xs

      pivot = MaglevUndefined
      xs.each { |o| pivot = o; break }
      return xs if pivot._equal?( MaglevUndefined)

      lmr = xs.group_by do |o|
        if o._equal?(pivot)
          0
        else
          yield(o, pivot)
        end
      end
      quicksort(lmr[-1], &prc) + lmr[0] + quicksort(lmr[1], &prc)
    end

    class SortedElement
      def initialize(val, sort_id)
        @value, @sort_id = val, sort_id
      end

      def value
        @value
      end
      def sort_id
        @sort_id
      end

      def <=>(other)
        @sort_id <=> other.sort_id
      end
    end

    # Sort#sort_by  in Enumerable2.rb , 
    #    for compile-time resolution of SortedElement

  end # ]
  Sort.__freeze_constants

  # :call-seq:
  #   enum.all?                     => true or false
  #   enum.all? { |obj| block }   => true or false
  #
  # Passes each element of the collection to the given block. The method
  # returns true if the block never returns false or nil. If the block is not
  # given, Ruby adds an implicit block of <tt>{ |obj| obj }</tt> (that is all?
  # will return true only if none of the collection members are
  # false or nil.)
  #
  #   %w[ant bear cat].all? { |word| word.length >= 3}   #=> true
  #   %w[ant bear cat].all? { |word| word.length >= 4}   #=> false
  #   [ nil, true, 99 ].all?                             #=> false

  def all?(&block)
    if block_given?
      each { |e| return false unless block.call(e) }
    else
      each { |e| return false unless e }
    end
    true
  end

  ##
  # :call-seq:
  #    enum.any? [{ |obj| block } ]   => true or false
  #
  # Passes each element of the collection to the given block. The method
  # returns true if the block ever returns a value other than false or nil. If
  # the block is not given, Ruby adds an implicit block of <tt>{ |obj| obj
  # }</tt> (that is any? will return true if at least one of the collection
  # members is not false or nil.
  #
  #   %w[ant bear cat].any? { |word| word.length >= 3}   #=> true
  #   %w[ant bear cat].any? { |word| word.length >= 4}   #=> true
  #   [ nil, true, 99 ].any?                             #=> true

  def any?(&block)
    if block_given?
      each { |o| return true if block.call(o) }
    else
      each { |o| return true if o } # for 1.8.7
    end
    false
  end

  ##
  # :call-seq:
  #   enum.collect { | obj | block }  => array
  #   enum.map     { | obj | block }  => array
  #
  # Returns a new array with the results of running +block+ once for every
  # element in +enum+.
  #
  #   (1..4).collect { |i| i*i }   #=> [1, 4, 9, 16]
  #   (1..4).collect { "cat"  }   #=> ["cat", "cat", "cat", "cat"]

  def collect(&block)
    ary = []
    if block_given?
      self.each { |o| ary << block.call(o) }
    else
      self.each { |o| ary << o }
    end
    ary
  end

  alias_method :map, :collect

  ##
  # :call-seq:
  #   enum.count(item)             => int
  #   enum.count { | obj | block } => int
  #
  # Returns the number of items in +enum+ that equal +item+ or for which
  # the block returns a true value.  Returns the number of all elements in
  # +enum+ if neither a block nor an argument is given.
  #
  #   ary = [1, 2, 4, 2]
  #   ary.count(2)          # => 2
  #   ary.count{ |x|x%2==0}  # => 3

  def count(item=MaglevUndefined, &block)
    seq = 0
    if item._equal?(MaglevUndefined)
      if block_given?
        self.each { |o| seq += 1 if block.call(o) }
      else
        self.each { |o| seq += 1 }
      end
    else
      self.each { |o| seq += 1 if item == o }
    end
    seq
  end

  def cycle(count=Fixnum__MAX, &block)
    if count._equal?(nil)
      cnt = Fixnum__MAX
    else
      cnt = Maglev::Type.coerce_to( count, Fixnum, :to_int)
    end
    unless block_given?
      # Returns an Enumerator
      return self.to_a.cycle(cnt)
    end
    if cnt <= 0
      return nil
    end
    self.to_a.cycle(cnt, &block)
  end

  def drop(count)
    cnt = Maglev::Type.coerce_to( count, Fixnum, :to_int)
    self.to_a.drop(cnt) 
  end

  def drop_while(&block)
    self.to_a.drop_while(&block)
  end

  def each_cons(count, &block)  # added for 1.8.7
    cnt = Maglev::Type.coerce_to( count, Fixnum, :to_int)
    unless cnt > 0
      raise TypeError, 'each_cons, arg must be > 0'
    end
    unless block_given?
      return ArrayCombinationEnumerator.new( cnt , self, :each_cons )
    end
    holder = [ 1 ]
    ea_res = holder.each { | unused_x | 
      array = []
      elements = self.to_a
      num_elem = elements.__size
      idx = 0
      last = num_elem - cnt
      while idx <= last
        sub_arr = elements[idx, cnt]
        block.call(sub_arr)
        idx += 1 
      end
    }
    if ea_res._not_equal?(holder)
      return ea_res  # break out of the block
    end
    nil
  end

  ##
  # :call-seq:
  #   enum.each_with_index { |obj, i| block }  -> enum
  #
  # Calls +block+ with two arguments, the item and its index, for
  # each item in +enum+.
  #
  #   hash = {}
  #   %w[cat dog wombat].each_with_index { |item, index|
  #     hash[item] = index
  #   }
  #
  #   p hash   #=> {"cat"=>0, "wombat"=>2, "dog"=>1}
  #
  def each_with_index(&block)
    unless block_given?
      # returns an Enumerator	for 1.8.7
      return self.to_a.each_with_index()
    end
    idx = 0
    self.each { |o| block.call(o, idx)
      idx += 1 
    }
    self
  end

  #  call-seq:
  #    e.each_slice(n) {...}
  #    e.each_slice(n)
  #
  #  Iterates the given block for each slice of <n> elements.  If no
  #  block is given, returns an enumerator.
  #
  #  e.g.:
  #      (1..10).each_slice(3) {|a| p a}
  #      # outputs below
  #      [1, 2, 3]
  #      [4, 5, 6]
  #      [7, 8, 9]
  #      [10]
  #
  def each_slice(count, &block)	# rewritten for 1.8.7
    slice_size = Maglev::Type.coerce_to( count, Integer, :to_int)
    unless slice_size > 0
      raise TypeError, 'each_slice, arg must be > 0'
    end
    unless block_given?
      return ArrayCombinationEnumerator.new( slice_size , self, :each_slice )
    end
    n = 0
    broke = false
    a_slice = Array.new(slice_size)
    ea_res = self.each { | elem |
      a_slice.__at_put(n, elem)
      n += 1
      if n._equal?(slice_size)
        broke = true
        block.call( a_slice )
        broke = false
        a_slice = Array.new(slice_size)
        n = 0
      end
    }
    if broke
      return ea_res # the &block did a break
    end
    if n > 0
      a_slice.size=(n)
      block.call( a_slice )
    end
    nil
  end

  def enum_cons(count)		 # added for 1.8.7
    return self.each_cons(count)
  end

  def enum_slice(count)		 # added for 1.8.7
    return self.each_slice(count)
  end

  def enum_with_index()		# added for 1.8.7
    return self.each_with_index()
  end
  
  ##
  # :call-seq:
  #   enum.detect(ifnone = nil) { | obj | block }  => obj or nil
  #   enum.find(ifnone = nil)   { | obj | block }  => obj or nil
  #
  # Passes each entry in +enum+ to +block+>. Returns the first for which
  # +block+ is not false.  If no object matches, calls +ifnone+ and returns
  # its result when it is specified, or returns nil
  #
  #   (1..10).detect  { |i| i % 5 == 0 and i % 7 == 0 }   #=> nil
  #   (1..100).detect { |i| i % 5 == 0 and i % 7 == 0 }   #=> 35

  def find(ifnone=nil, &block)
    unless block_given?
      return FirstEnumerator.new(self, :find, ifnone) # for 1.8.7
    end
    each { |o| return o if block.call(o) }
    ifnone.call if ifnone
  end

  alias_method :detect, :find

  ##
  # :call-seq:
  #   enum.find_index()   { | obj | block }  => int
  #
  # Passes each entry in +enum+ to +block+. Returns the index for the first
  # for which +block+ is not false. If no object matches, returns
  # nil.
  #
  #   (1..10).find_index  { |i| i % 5 == 0 and i % 7 == 0 }   #=> nil
  #   (1..100).find_index { |i| i % 5 == 0 and i % 7 == 0 }   #=> 35

  def find_index(object=MaglevUndefined, &block)
    idx = -1 
    if object._equal?(MaglevUndefined)
      unless block_given?
        return FirstEnumerator.new(self, :first_index) # for 1.8.7
      end
      self.each { |o| idx += 1; return idx if block.call(o) }
    else
      # ignore block when first arg is given
      self.each { |o| idx += 1; return idx if o == object }
    end  
    nil
  end

  ##
  # :call-seq:
  #   enum.find_all { | obj | block }  => array
  #   enum.select   { | obj | block }  => array
  #
  # Returns an array containing all elements of +enum+ for which +block+ is
  # not false (see also Enumerable#reject).
  #
  #   (1..10).find_all { |i|  i % 3 == 0 }   #=> [3, 6, 9]

  def find_all(&block)
    unless block_given?
      return ArraySelectEnumerator.new(self, :find_all) # for 1.8.7
    end
    ary = []
    self.each { |o|
      if block.call(o)
        ary << o
      end
    }
    ary
  end

  alias_method :select, :find_all

  ##
  # :call-seq:
  #   enum.first      => obj or nil
  #   enum.first(n)   => an_array
  #
  # Returns the first element, or the first +n+ elements, of the enumerable.
  # If the enumerable is empty, the first form returns nil, and the second
  # form returns an empty array.

  def first(count=MaglevUndefined)
    if count._equal?(MaglevUndefined)
      return self.first()
    end
    # return Array of first count elements of the enumeration
    cnt = Maglev::Type.coerce_to(count, Fixnum, :to_int)
    if cnt <= 0
      if cnt._equal?(0)
        return []
      end
      raise ArgumentError, 'negative count'
    end
    arr = []
    n = 0
    self.each { |o| 
      arr << o
      n += 1
      if n >= cnt
        return arr
      end
    }
    return arr
  end

  def first()
    # returns first element of the enumeration , or nil 
    self.each { |o| return o }
    nil
  end

  # :call-seq:
  #   enum.include?(obj)     => true or false
  #   enum.member?(obj)      => true or false
  #
  # Returns true if any member of +enum+ equals +obj+. Equality is tested
  # using #==.
  #
  #   IO.constants.include? "SEEK_SET"          #=> true
  #   IO.constants.include? "SEEK_NO_FURTHER"   #=> false

  def include?(obj)
    each { |o| return true if o == obj }
    false
  end

  alias_method :member?, :include?

  ##
  # :call-seq:
  #   enum.inject(initial) { | memo, obj | block }  => obj
  #   enum.inject          { | memo, obj | block }  => obj
  #   enum.inject(initial, binary_op_sym)
  #   enum.inject(binary_op_sym) 
  #
  # Combines the elements of +enum+ by applying the block to an accumulator
  # value (+memo+) and each element in turn. At each step, +memo+ is set
  # to the value returned by the block. The first form lets you supply an
  # initial value for +memo+. The second form uses the first element of the
  # collection as a the initial value (and skips that element while
  # iterating).
  #
  # Sum some numbers:
  #
  #   (5..10).inject { |sum, n| sum + n }              #=> 45
  #
  # Multiply some numbers:
  #
  #   (5..10).inject(1) { |product, n| product * n }   #=> 151200
  #
  # Find the longest word:
  #
  #   longest = %w[ cat sheep bear ].inject do |memo,word|
  #      memo.length > word.length ? memo : word
  #   end
  #
  #   longest                                         #=> "sheep"
  #
  # Find the length of the longest word:
  #
  #   longest = %w[ cat sheep bear ].inject(0) do |memo,word|
  #      memo >= word.length ? memo : word.length
  #   end
  #
  #   longest                                         #=> 5

  def inject(initial=MaglevUndefined, bin_op=MaglevUndefined, &block) # added for 1.8.7
    uu = MaglevUndefined
    if bin_op._equal?(uu)
      if initial._equal?(uu)
        self.inject(&block)
      elsif block_given?
        self.inject(initial, &block) 
      else
	self.inject(initial) # arg 1 is a bin_op
      end
    else
      # per specs, ignore block if 2 args given
      self.inject(initial, bin_op)
    end
  end

  def inject(initial, &block)
    memo = initial
    self.each { |o|
      memo = block.call(memo, o) 
    }
    memo
  end

  def inject(&block)
    uu = MaglevUndefined
    memo = uu
    each { |o|
      if memo._equal?(uu)
        memo = o
      else
        memo = block.call(memo, o)
      end
    }
    memo._equal?(uu) ? nil : memo
  end

  def inject(initial, binary_op_sym) # added for 1.8.7
    memo = initial
    self.each { |o|
      memo = memo.__send__(binary_op_sym, o)    
    }
    memo
  end

  def inject(binary_op_sym) # added for 1.8.7
    uu = MaglevUndefined
    memo = uu
    each { |o|
      if memo._equal?(uu) 
        memo = o
      else
        memo = memo.__send__(binary_op_sym, o)
      end
    } 
    memo._equal?(uu) ? nil : memo
  end

  alias_method :reduce, :inject  # added for 1.8.7

  ##
  # :call-seq:
  #   enum.grep(pattern)                   => array
  #   enum.grep(pattern) { | obj | block } => array
  #
  # Returns an array of every element in +enum+ for which <tt>Pattern ===
  # element</tt>. If the optional +block+ is supplied, each matching element
  # is passed to it, and the block's result is stored in the output array.
  #
  #   (1..100).grep 38..44   #=> [38, 39, 40, 41, 42, 43, 44]
  #   c = IO.constants
  #   c.grep(/SEEK/)         #=> ["SEEK_END", "SEEK_SET", "SEEK_CUR"]
  #   res = c.grep(/SEEK/) { |v| IO.const_get(v) }
  #   res                    #=> [2, 0, 1]

  def grep(pattern, &block)
    ary = []
    if block_given?
      if pattern._isRegexp
        saveTilde = block.__fetchRubyVcGlobal(0);
        begin
	  self.each { |elem|
	    if elem._isString  # inline Regexp#===
	      md = pattern.__search(elem, 0, nil)
	      if md && md.begin(0)
		block.__setRubyVcGlobal(0, md)
		ary.__push( block.call( elem ) )
              end
            end
	  }
        ensure
          block.__setRubyVcGlobal(0, saveTilde)
        end
      else
        self.each { |o|
          if pattern === o
            ary.__push(block.call(o))
          end
        }
      end
    else
      self.each { |o|
        if pattern === o
          ary.__push(o)
        end
      }
    end
    ary
  end

  ##
  # :call-seq:
  #   enum.group_by { | obj | block }  => a_hash
  #
  # Returns a hash, which keys are evaluated result from the block, and values
  # are arrays of elements in +enum+ corresponding to the key.
  #
  #    (1..6).group_by { |i| i%3}   #=> {0=>[3, 6], 1=>[1, 4], 2=>[2, 5]}

  def group_by(&block)
    unless block_given?
      return ArrayEnumerator.new( self.to_a, :group_by ) # for 1.8.7
    end
    h = {}
    self.each { |o|
      key = block.call(o)
      if h.key?(key)
        h[key] << o
      else
        h[key] = [o]
      end
    }
    h
  end

  ##
  # :call-seq:
  #   enum.max                   => obj
  #   enum.max { |a,b| block }   => obj
  #
  # Returns the object in +enum+ with the maximum value. The first form
  # assumes all objects implement Comparable; the second uses the block to
  # return <tt>a <=> b</tt>.
  #
  #    a = %w[albatross dog horse]
  #    a.max                                  #=> "horse"
  #    a.max { |a,b| a.length <=> b.length }   #=> "albatross"
  #
  def max(&block)
    un_defined = MaglevUndefined
    v = un_defined
    if block_given?
      self.each { |o|
	if v._equal?(un_defined)
	  v = o
	else
	  c = block.call(o, v) 
	  if c._equal?(nil)
	    raise ArgumentError, "comparison of #{o.class} with #{v} failed"
	  end
	  if c > 0
	    v = o
	  end
	end
      } 
    else  # block not given, rewritten for 1.8.7
      self.each { |o|
	if v._equal?(un_defined)
	  v = o
	else
	  c =  o <=> v
	  if c._equal?(nil)
	    raise ArgumentError, "comparison of #{o.class} with #{v} failed"
	  end
	  if c > 0
	    v = o
	  end
	end
      } 
    end
    v._equal?(un_defined) ? nil : v
  end


  ##
  # :call-seq:
  #   enum.max_by { | obj| block }   => obj
  #
  # Uses the values returned by the given block as a substitute for the real
  # object to determine what is considered the largest object in +enum+ using
  # <tt>lhs <=> rhs</tt>. In the event of a tie, the object that appears first
  # in #each is chosen. Returns the "largest" object or nil if the enum is
  # empty.
  #
  #   a = %w[albatross dog horse]
  #   a.max_by { |x| x.length }   #=> "albatross"

  def max_by(&block)
    unless block_given?
      return FirstEnumerator.new(self.to_a, :max_by) # for 1.8.7
    end
    un_defined = MaglevUndefined
    max_obj = un_defined
    max_value = un_defined

    self.each { |o|
      val = block.call(o)
      if max_obj._equal?(un_defined) or (max_value <=> val) < 0
        max_obj = o
        max_value = val
      end
    }
    max_obj._equal?(un_defined) ? nil : max_obj
  end

  ##
  # :call-seq:
  #   enum.min                    => obj
  #   enum.min { | a,b | block }  => obj
  #
  # Returns the object in +enum+ with the minimum value. The first form
  # assumes all objects implement Comparable; the second uses the block to
  # return <tt>a <=> b</tt>.
  #
  #   a = %w[albatross dog horse]
  #   a.min                                  #=> "albatross"
  #   a.min { |a,b| a.length <=> b.length }   #=> "dog"

  def min(&block)
    un_defined = MaglevUndefined
    v = un_defined
    if block_given?
      self.each { |o|
	if v._equal?(un_defined)
	  v = o
	else
	  c = block.call(o, v) 
	  if c._equal?(nil)
	    raise ArgumentError, "comparison of #{o.class} with #{v} failed"
	  end
	  if c < 0
	    v = o
	  end
	end
      } 
    else 	# no block given, rewritten for 1.8.7
      self.each { |o|
	if v._equal?(un_defined)
	  v = o
	else
	  c =  o <=> v
	  if c._equal?(nil)
	    raise ArgumentError, "comparison of #{o.class} with #{v} failed"
	  end
	  if c < 0
	    v = o
	  end
	end
      } 
    end
    v._equal?(un_defined) ? nil : v
  end

  ##
  # :call-seq:
  #   enum.min_by { |obj| block }   => obj
  #
  # Uses the values returned by the given block as a substitute for the real
  # object to determine what is considered the smallest object in +enum+ using
  # <tt>lhs <=> rhs</tt>. In the event of a tie, the object that appears first
  # in #each is chosen. Returns the "smallest" object or nil if the enum is
  # empty.
  #
  #   a = %w[albatross dog horse]
  #   a.min_by { |x| x.length }   #=> "dog"

  def min_by(&block)
    unless block_given?
      return ArrayEnumerator.new( self.to_a, :group_by ) # for 1.8.7
    end
    un_defined = MaglevUndefined
    min_obj = un_defined
    min_value = un_defined

    self.each { |o|
      val = block.call(o)
      if min_obj._equal?(un_defined) or (min_value <=> val) > 0
        min_obj = o
        min_value = val
      end
    }
    min_obj._equal?(un_defined) ? nil : min_obj
  end

  # minmax added for 1.8.7

  def minmax(&block)
    un_defined = MaglevUndefined
    mino = un_defined
    maxo = un_defined
    if block_given?
      self.each { |o|
	if mino._equal?(un_defined)
	  mino = o
	  maxo = o
	else
	  c = block.call(o, mino) 
	  if c._equal?(nil)
	    raise ArgumentError, "comparison of #{o.class} with #{mino} failed"
	  end
	  if c < 0
	    mino = o
	  end
	  c = block.call(o, maxo) 
	  if c._equal?(nil)
	    raise ArgumentError, "comparison of #{o.class} with #{maxo} failed"
	  end
	  if c > 0
	    maxo = o
	  end
	end
      } 
    else
      self.each { |o|
	if mino._equal?(un_defined)
	  mino = o
	  maxo = o
	else 
	  c = o <=> mino
	  if c._equal?(nil)
	    raise ArgumentError, "comparison of #{o.class} with #{mino} failed"
	  end
	  if c < 0
	    mino = o
	  end
	  c = o <=> maxo
	  if c._equal?(nil)
	    raise ArgumentError, "comparison of #{o.class} with #{maxo} failed"
	  end
	  if c > 0
	    maxo = o
	  end
	end
      } 
    end
    mino._equal?(un_defined) ? [ nil, nil] : [ mino, maxo ]
  end

  def minmax_by(&block)  # added for 1.8.7
    # block should be a one-argument block
    unless block_given?
      return FirstEnumerator.new(self.to_a, :minmax_by ) # for 1.8.7
    end
    un_defined = MaglevUndefined
    minv = un_defined
    mino = nil
    maxv = un_defined
    maxo = nil
    self.each { |o|
      if minv._equal?(un_defined)
        minv = block.call(o)
        maxv = minv
        mino = o
        maxo = o
      else
        v = block.call(o)
        c =  v <=> minv
        if c._equal?(nil)
          raise ArgumentError, "comparison of #{v.class} with #{minv} failed"
        end
        if c < 0
          minv = v
          mino = o
        end
        c = v <=> maxv
        if c._equal?(nil)
          raise ArgumentError, "comparison of #{o.class} with #{maxv} failed"
        end
        if c > 0
          maxv = v
          maxo = o
        end
      end
    } 
    minv._equal?(un_defined) ? [ nil, nil] : [ mino, maxo ]
  end

  ##
  # :call-seq:
  #   enum.none?                   => true or false
  #   enum.none? { |obj| block }   => true or false
  #
  # Passes each element of the collection to the given block. The method
  # returns true if the block never returns true for all elements. If the
  # block is not given, none? will return true only if any of the collection
  # members is true.
  #
  #    %w{ant bear cat}.none? { |word| word.length == 4}   #=> true
  #    %w{ant bear cat}.none? { |word| word.length >= 4}   #=> false
  #    [ nil, true, 99 ].none?                             #=> true

  def none?(&block)
    if block_given?
      self.each { |o| 
        if block.call(o) 
          return false
        end
      }
    else
      self.each { |o| 
        if o
          return false
        end
      }
    end
    true
  end


  ##
  # :call-seq:
  #   enum.one?                   => true or false
  #   enum.one? { |obj| block }   => true or false
  #
  # Passes each element of the collection to the given block. The method
  # returns true if the block returns true exactly once. If the block is not
  # given, one? will return true only if exactly one of the collection members
  # are true.
  #
  #   %w[ant bear cat].one? { |word| word.length == 4}   #=> true
  #   %w[ant bear cat].one? { |word| word.length >= 4}   #=> false
  #   [ nil, true, 99 ].one?                             #=> true

  def one?(&block)
    count = 0
    if block_given?
      each { |o| count += 1 if block.call(o) }
    else
      each { |o| count += 1 if o }   # for 1.8.7
    end
    count == 1
  end


  ##
  # :call-seq:
  #   enum.partition { | obj | block }  => [ true_array, false_array ]
  #
  # Returns two arrays, the first containing the elements of +enum+ for which
  # the block evaluates to true, the second containing the rest.
  #
  #   (1..6).partition { |i| (i&1).zero?}   #=> [[2, 4, 6], [1, 3, 5]]

  def partition(&block)
    unless block_given?
      return ArraySelectEnumerator.new(self, :partition) # for 1.8.7
    end
    left = []
    right = []
    each { |o| block.call(o) ? left.push(o) : right.push(o) }
    return [left, right]
  end

  ##
  # :call-seq:
  #   enum.reject { | obj | block }  => array
  #
  # Returns an array for all elements of +enum+ for which +block+ is false
  # (see also Enumerable#find_all).
  #
  #    (1..10).reject { |i|  i % 3 == 0 }   #=> [1, 2, 4, 5, 7, 8, 10]

  def reject(&block)
    unless block_given?
      return ArraySelectEnumerator.new(self, :reject) # for 1.8.7
    end
    ary = []
    each { |o|
      unless block.call(o)
        ary << o
      end
    }
    ary
  end

  def reverse_each(&block) 	# added for 1.8.7
    self.to_a.reverse_each(&block)
  end

  ##
  # :call-seq:
  #   enum.sort                     => array
  #   enum.sort { | a, b | block }  => array
  #
  # Returns an array containing the items in +enum+ sorted, either according
  # to their own <tt><=></tt> method, or by using the results of the supplied
  # block. The block should return -1, 0, or +1 depending on the comparison
  # between +a+> and +b+.  As of Ruby 1.8, the method Enumerable#sort_by
  # implements a built-in Schwartzian Transform, useful when key computation
  # or comparison is expensive..
  #
  #   %w(rhea kea flea).sort         #=> ["flea", "kea", "rhea"]
  #   (1..10).sort { |a,b| b <=> a}  #=> [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

  def sort(&block)
    self.sorter.sort(self, &block)
  end

  # def sorter; end # in Enumerable2.rb

  ##
  # :call-seq:
  #   enum.sort_by { | obj | block }    => array
  #
  # Sorts +enum+ using a set of keys generated by mapping the
  # values in +enum+ through the given block.
  #
  #   %w{ apple pear fig }.sort_by { |word| word.length}
  #   #=> ["fig", "pear", "apple"]
  #
  # The current implementation of sort_by generates an array of tuples
  # containing the original collection element and the mapped value. This makes
  # sort_by fairly expensive when the keysets are simple
  #
  #   require 'benchmark'
  #   include Benchmark
  #
  #   a = (1..100000).map {rand(100000)}
  #
  #   bm(10) do |b|
  #     b.report("Sort")    { a.sort }
  #     b.report("Sort by") { a.sort_by { |a| a} }
  #   end
  #
  # produces:
  #
  #   user     system      total        real
  #   Sort        0.180000   0.000000   0.180000 (  0.175469)
  #   Sort by     1.980000   0.040000   2.020000 (  2.013586)
  #
  # However, consider the case where comparing the keys is a non-trivial
  # operation. The following code sorts some files on modification time
  # using the basic sort method.
  #
  #   files = Dir["#"]
  #   sorted = files.sort { |a,b| File.new(a).mtime <=> File.new(b).mtime}
  #   sorted   #=> ["mon", "tues", "wed", "thurs"]
  #
  # This sort is inefficient: it generates two new File objects during every
  # comparison. A slightly better technique is to use the Kernel#test method
  # to generate the modification times directly.
  #
  #   files = Dir["#"]
  #   sorted = files.sort { |a,b|
  #     test(?M, a) <=> test(?M, b)
  #   }
  #   sorted   #=> ["mon", "tues", "wed", "thurs"]
  #
  # This still generates many unnecessary Time objects. A more efficient
  # technique is to cache the sort keys (modification times in this case)
  # before the sort. Perl users often call this approach a Schwartzian
  # Transform, after Randal Schwartz. We construct a temporary array, where
  # each element is an array containing our sort key along with the filename.
  # We sort this array, and then extract the filename from the result.
  #
  #   sorted = Dir["#"].collect { |f|
  #      [test(?M, f), f]
  #   }.sort.collect { |f| f[1] }
  #   sorted   #=> ["mon", "tues", "wed", "thurs"]
  #
  # This is exactly what sort_by does internally.
  #
  #   sorted = Dir["#"].sort_by { |f| test(?M, f)}
  #   sorted   #=> ["mon", "tues", "wed", "thurs"]

  def sort_by(&block)
    unless block_given?
      return FirstEnumerator.new(self, :sort_by)  # for 1.8.7
    end
    self.sorter.sort_by(self, &block)
  end
 
  def take(count)	# added for 1.8.7
    cnt = Maglev::Type.coerce_to( count, Fixnum, :to_int)
    if cnt <= 0
      if cnt < 0
        raise ArgumentError, 'arg to take must be >= 0'
      end
      return []
    end
    arr = [] 
    res_size = 0
    self.each { |o|
      arr << o
      res_size += 1
      if res_size._equal?(cnt)
        return arr
      end
    }
    arr
  end

  def take_while(&block)  # added for 1.8.7
    unless block_given?
      # returns an Enumerator
      return self.to_a.take_while()
    end
    brk_flag = false
    arr = []
    darr = []
    blk_res = nil
    n = 0
    broke = false
    enum_res = self.each { |elem|
      n += 1
      broke = true
      blk_res = block.call(elem)
      broke = false
      darr << blk_res
      unless blk_res
        break
      end
      arr << elem
    }
    if broke
      return enum_res  # the argument block did a break
    end
    arr
  end

  ##
  # :call-seq:
  #   enum.to_a      =>    array
  #   enum.entries   =>    array
  #
  # Returns an array containing the items in +enum+.
  #
  #   (1..7).to_a                       #=> [1, 2, 3, 4, 5, 6, 7]
  #   { 'a'=>1, 'b'=>2, 'c'=>3 }.to_a   #=> [["a", 1], ["b", 2], ["c", 3]]

  def to_a(*args)
    ary = []
    self.each(*args) { |o| 
      ary << o 
    }
    ary
  end

  def to_a
    # common variant, to avoid bridge method
    ary = []
    self.each { |o| 
      ary << o 
    }
    ary
  end

  alias_method :entries, :to_a

  ##
  # :call-seq:
  #    enum.zip(arg, ...)                   => array
  #    enum.zip(arg, ...) { |arr| block }   => nil
  #
  # Converts any arguments to arrays, then merges elements of +enum+ with
  # corresponding elements from each argument. This generates a sequence of
  # enum#size +n+-element arrays, where +n+ is one more that the count of
  # arguments. If the size of any argument is less than enum#size, nil values
  # are supplied. If a block given, it is invoked for each output array,
  # otherwise an array of arrays is returned.
  #
  #   a = [ 4, 5, 6 ]
  #   b = [ 7, 8, 9 ]
  #
  #   (1..3).zip(a, b)      #=> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
  #   "cat\ndog".zip([1])   #=> [["cat\n", 1], ["dog", nil]]
  #   (1..3).zip            #=> [[1], [2], [3]]

  def zip(*args)
    result = []
    args = args.map { |a| a.to_a }
    each_with_index do |o, i|
      result << args.inject([o]) { |ary, a| ary << a[i] }
      yield(result.last) if block_given?
    end
    result unless block_given?
  end
end
Enumerable.__freeze_constants

