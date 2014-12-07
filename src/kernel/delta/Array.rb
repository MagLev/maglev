class Array
  # The include of Enumerable is necessary so that the specs that ensure
  # Array includes Enumerable pass.  We immediately override most of
  # Enumerable (but *do* pick up on the sort).
  include Enumerable

  # These are the overrides of Enumerable for performance reasons
  #
  # TODO: After Enumerable is implemented, we should re-visit these and
  # ensure only the methods critical for performane or Array specific
  # implementations are implemented here.

  def all?(&block)
    n = 0
    lim = self.__size
    if block_given?
      while n < lim
        unless yield(self.__at(n)) ; return false ; end
        n = n + 1
      end
    else
      while n < lim
        unless self.__at(n) ; return false ; end
        n = n + 1
      end
    end
    true
  end

  # Equivalent to Array#delete_if, but returns nil if no changes were made.
  #
  # Note: we do not use Smalltalk removeAllSuchThat:, as Smalltalk expects
  # only boolean values to be returned by the block, but Ruby allows nil
  # for false and anything but nil or false is considered true.
  def reject!(&block)
    unless block_given?
      return ArrayEnumerator.new(self, :reject! ) # for 1.8.7
    end
    a = reject(&block)
    if a.size == self.__size
      nil # no changes, return nil
    else
      replace(a)
      self
    end
  end

  def any?(&block)
    n = 0
    lim = self.__size
    if block_given?
      while n < lim
        if yield(self.__at(n)) ; return true ; end
        n = n + 1
      end
    else
      while n < lim
        if self.__at(n) ; return true ; end
        n = n + 1
      end
    end
    false
  end

  def collect(&block)
    unless block_given?
      return self.dup
    end
    sz = self.__size
    arr = Array.new(sz)  # Array, not self.class
    i = 0
    enum_res = self.each { | elem |  # use self.each to handle break in block
      arr.__at_put(i, block.call( elem ))
      i += 1
    }
    if i < sz
      return enum_res 
    end
    arr
  end

  # When invoked with a block, yields all combinations of length n of elements
  # from ary and then returns ary itself. Even though the implementation makes
  # no guarantees about the order in which the combinations are yielded, we copy MRI.
  # When invoked without a block, returns an enumerator object instead.
  #
  def combination(num, &block)  # added for 1.8.7 , modified from Rubinius
    num = Maglev::Type.coerce_to( num, Fixnum, :to_int)
    unless block_given?
      return ArrayCombinationEnumerator.new(num, self, :combination) # for 1.8.7
    end
    my_size = self.__size 
    return self unless num >= 0 && num <= my_size # unless (0..size).include?(num)
    # Implementation note: slightly tricky.
                                             # Example: self = 1..7, num = 3
    picks = (0...num).__integer_to_a                   # picks start at 0, 1, 2
    max = ((my_size - num)... my_size).__integer_to_a   # max (index for a given pick) is [4, 5, 6]
    pick_max_pairs = picks.zip(max).reverse  # pick_max_pairs = [[2, 6], [1, 5], [0, 4]]
    pick_mp_siz = pick_max_pairs.__size
    loop do
      block.call( self.values_at(*picks) ) 
      move = nil
      k = 0
      while k < pick_mp_siz
        apair = pick_max_pairs.__at(k)
        apick = apair.__at(0)
        amax = apair.__at(1)
        if (picks.__at(apick) < amax)
          move = apick
          break
        end
        k += 1
      end
      if move._equal?(nil)
        return self  # all of pick_max_pairs have been processed
      end
      new_index = picks.__at(move) + 1
      picks.__at_put(move...num, (new_index...(new_index+num-move)).__integer_to_a )
    end
  end

  def detect(ifnone=nil, &block)
    unless block_given?
      return FirstEnumerator.new(self, :detect, ifnone) # for 1.8.7
    end
    n = 0
    lim = self.__size
    while n < lim
      elem = self.__at(n)
      if block.call(elem)
        return elem
      end
      n = n + 1
    end
    ifnone.call if ifnone
  end

  alias entries to_a

  alias find  detect

  def find_all(&block)
    unless block_given?
      return ArrayEnumerator.new(self, :find_all ) # for 1.8.7
    end
    result = []  # not self.class.new
    i = 0
    lim = self.__size
    while i < lim
      el = self.__at(i)
      result.__push( el ) if block.call(el)
      i += 1
    end
    result
  end

  def grep(pattern, &block)
    result = []  # not self.class.new
    i = 0
    lim = self.__size
    if block_given?
      if pattern._isRegexp
        saveTilde = block.__fetchRubyVcGlobal(0)
        begin
          while i < lim
            elem = self.__at(i)
            if elem._isString  # inline  Regexp#===
              md = pattern.__search(elem, 0, nil)
              if md && md.begin(0)
                block.__setRubyVcGlobal(0, md)
                result.__push( block.call( elem ) )
              end
            end
            i += 1
          end
        ensure
          block.__setRubyVcGlobal(0, saveTilde)
        end
      else
        while i < lim
          elem = self.__at(i)
          if pattern === elem
            result.__push( block.call( elem))
          end
          i += 1
        end
      end
    else
      while i < lim
        elem = self.__at(i)
        if pattern === elem
          result.__push(elem)
        end
        i += 1
      end
    end
    result
  end

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
    accum = initial
    my_size = self.__size
    n = 0
    while n < my_size
      accum = block.call(accum, self.__at(n))
      n = n + 1
    end
    accum
  end

  def inject(&block)
    my_size = self.__size
    accum = nil
    n = 0
    if my_size > 0
      accum = self.__at(0)
      n = 1
      while n < my_size
        accum = block.call(accum, self.__at(n))
        n = n + 1
      end
    end
    accum
  end

  def inject(initial, binary_op_sym) # added for 1.8.7
    memo = initial
    my_size = self.__size
    n = 0
    while n < my_size
      memo = memo.__send__(binary_op_sym, self.__at(n))
      n = n + 1
    end
    memo
  end

  def inject(binary_op_sym) # added for 1.8.7
    un_defined = MaglevUndefined
    memo = un_defined
    my_size = self.__size
    n = 0
    while n < my_size
      o = self.__at(n)
      if memo._equal?(un_defined)
        memo = o
      else
        memo = memo.__send__(binary_op_sym, o)
      end
      n = n +1
    end
    memo._equal?(un_defined) ? nil : memo
  end


  # Generates a string from converting all elements of
  # the Array to strings, inserting a separator between
  # each. The separator defaults to $,. Detects recursive
  # Arrays.
  def join(s = MaglevUndefined)
    # this is mostly Rubinius code, but modified for our API
    # Put in delta since we need the recursion guard found in common.
    my_size = self.__size
    return "" if my_size._equal?(0)
    if s._equal?(MaglevUndefined)
      sep = $,
    elsif s._equal?(nil)
       sep = nil
    else
      sep = s.to_str # let any NoMethodError be seen by caller
    end

    out = ""
#    out.taint if sep.tainted? or self.tainted?
    i = 0
    ts = Thread.__recursion_guard_set
    while (i < my_size)
      elem = at(i)
      out << sep unless (sep._equal?(nil) || i == 0)

      if elem._isArray
	added = ts.__add_if_absent(self)
	begin
          if ts.include?(elem)
            out << "[...]"
          else
            out << elem.join(sep)
          end
	ensure
          if added
	   ts.remove(self)
	  end
        end
      else
        out << elem.to_s
#        out.taint if elem.tainted? and not out.tainted?
      end
      i += 1
    end
    out
  end

  alias map collect

  def max(&block)
    lim = self.__size
    if lim._equal?(0)
      return nil 
    end
    max_v = self.__at(0)
    i = 1
    if block_given?
      while i < lim
	o = self.__at(i)
	comp = block.call(o, max_v)
	if comp._equal?(nil)
	  raise ArgumentError, "comparison of #{o.class} with #{max_v} failed" 
	end
	max_v = o if comp > 0
	i += 1
      end
    else
      while i < lim
        o = self.__at(i)
        max_v = o if (o <=> max_v) > 0
        i += 1
      end
    end
    max_v
  end

  primitive 'member?', 'includes:'

  def min(&block)
    lim = self.__size
    return nil if lim._equal?(0)
    min_v = self.__at(0)
    i = 1
    if block_given?
      while i < lim
        o = self.__at(i)
        comp = block.call(o, min_v)
        if comp._equal?(nil)
          raise ArgumentError, "comparison of #{o.class} with #{min_v} failed" 
        end
        min_v = o if comp < 0
        i += 1
      end
    else
      while i < lim
        o = self.__at(i)
        min_v = o if (o <=> min_v) < 0
        i += 1
      end
    end
    min_v
  end

  def partition(&block)
    t = []
    f = []
    i = 0
    lim = self.__size
    while i < lim
      el = self.__at(i)
      if block.call(el)
        t.__push( el )
      else
        f.__push( el )
      end
      i += 1
    end
    [t,f]  # not self.class .new
  end

    ##
  #  call-seq:
  #     ary.permutation { |p| block }          -> array
  #     ary.permutation                        -> enumerator
  #     ary.permutation(n) { |p| block }       -> array
  #     ary.permutation(n)                     -> enumerator
  #
  #  When invoked with a block, yield all permutations of length <i>n</i>
  #  of the elements of <i>ary</i>, then return the array itself.
  #  If <i>n</i> is not specified, yield all permutations of all elements.
  #  The implementation makes no guarantees about the order in which
  #  the permutations are yielded.
  #
  #  When invoked without a block, return an enumerator object instead.
  #
  #  Examples:
  #
  #     a = [1, 2, 3]
  #     a.permutation.to_a     #=> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
  #     a.permutation(1).to_a  #=> [[1],[2],[3]]
  #     a.permutation(2).to_a  #=> [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
  #     a.permutation(3).to_a  #=> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
  #     a.permutation(0).to_a  #=> [[]] # one permutation of length 0
  #     a.permutation(4).to_a  #=> []   # no permutations of length 4
  #
  def permutation(num=nil, &block)  # added in 1.8.7, from Rubinius
    my_size = self.__size
    if num._not_equal?(nil)
      num = Maglev::Type.coerce_to( num, Fixnum, :to_int )
    end
    unless block_given?
      return ArrayCombinationEnumerator.new(num, self, :permutation)
    end
    if num._equal?(nil)
      num = my_size
    end
    if num < 0 || my_size < num
      # no permutations, yield nothing
    elsif num._equal?( 0 )
      # exactly one permutation: the zero-length array
      block.call( [] )
    elsif num._equal?( 1 )
      # this is a special, easy case
      self.each { |val| block.call( [val] )  }
    else
      # this is the general case
      p = Array.new(num)    # not self.class .new
      used = String.__new(my_size)
      self.__permute__(num, p, 0, used, &block)
    end
    self
  end

  def __permute__(num, p, index, used, &block)
    # Recursively compute permutations of r elements of the set [0..n-1].
    # When we have a complete permutation of array indexes, copy the values
    # at those indexes into a new array and yield that array.
    #
    # num: the number of elements in each permutation
    # p: the array (of size num) that we're filling in
    # index: what index we're filling in now
    # used: a String of 0/1, whether a given index is already used
    #
    # Note: not as efficient as could be for big num.
    self.__size.times { |i|
      unless used.__at(i)._equal?(1)
        p.__at_put(index, i)
        if index < num-1
          used.__at_put(i, 1)
          self.__permute__(num, p, index+1, used, &block)
          used.__at_put(i, 0)
        else
          block.call( self.values_at(*p) )
        end
      end
    }
  end
  private :__permute__

  # Returns an array of all combinations of elements from all arrays.
  # The length of the returned array is the product of the length of
  # ary and the argument arrays
  #
  def product(*args)  # added in 1.8.7 # Rubinius code rewritten
    n = 0
    nargs = args.__size
    args_res = Array.new(nargs + 2)   
    args_res.__at_put(0, self)
    while n < nargs
      an_arg = Maglev::Type.coerce_to(args.__at(n), Array, :to_ary)
      n += 1
      args_res.__at_put(n,  an_arg)
    end
    result = []  # not self.class .new
    args_res.__at_put(-1, result )
    idxs = Array.new(nargs + 1, 0)
    self.__product(0, args_res, idxs)
    result
  end

  def __product(k, args_res, idxs)
    klast = args_res.__size - 2
    if k == klast
      t_res = Array.new(klast+1)
      m = 0
      while m < k
        t_res.__at_put(m, args_res.__at(m).__at( idxs.__at(m)) )
        m += 1
      end
      last_arr = args_res.__at(k)
      result = args_res.__at(klast + 1)
      n_lim = last_arr.__size 
      n = 0
      while n < n_lim
        # vary over the last input element of args_res
        a_res = t_res.dup
        a_res.__at_put(k,  last_arr.__at(n))
        result.__push( a_res  )
        n += 1
      end 
    else
      # vary over args_res[k]
      an_arg = args_res.__at(k)
      j = 0
      lim = an_arg.__size
      next_k = k + 1
      while j < lim
        idxs.__at_put(k, j)
        # recurse to vary over the next element of args_res
        self.__product(next_k, args_res, idxs)
        j += 1
      end
    end 
  end

  def reject(&block)
    unless block_given?
      return ArrayEnumerator.new(self, :reject ) # for 1.8.7
    end
    result = self.class.new
    i = 0
    lim = self.__size
    while i < lim
      result.__push(self.__at(i)) unless block.call(self.__at(i))
      i += 1
    end
    result
  end

  alias select find_all

  # Returns a new array with elements of this array shuffled.
  def shuffle()		# added in 1.8.7
    self.dup.shuffle!
  end

  # Shuffles elements in self in place.
  def shuffle!()   # added in 1.8.7  , from Rubinius, modified
    i = 0
    my_size = self.__size
    while i < my_size 
      r = i + rand(my_size - i)
      val_a = self.__at(i)
      val_b = self.__at(r)
      self.__at_put(i, val_b) 
      self.__at_put(r, val_a) 
      i += 1
    end
    self
  end

  # sort: is listed in the Pick Axe book under both Array and Enumerable,
  # and implemented here in the Array section above.

  # PickAxe p 457 documents that sort_by uses Schwartzian Transform
  # sort_by  implemented in Array section above

  # to_a: Implemented above in the Array section


  #  call-seq:
  #     array.zip(arg, ...)                   -> an_array
  #     array.zip(arg, ...) {| arr | block }  -> nil
  #
  #  Converts any arguments to arrays, then merges elements of
  #  <i>self</i> with corresponding elements from each argument. This
  #  generates a sequence of <code>self.size</code> <em>n</em>-element
  #  arrays, where <em>n</em> is one more that the count of arguments. If
  #  the size of any argument is less than <code>enumObj.size</code>,
  #  <code>nil</code> values are supplied. If a block given, it is
  #  invoked for each output array, otherwise an array of arrays is
  #  returned.
  #
  #     a = [ 4, 5, 6 ]
  #     b = [ 7, 8, 9 ]
  #     [1,2,3].zip(a, b)      #=> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
  #     [1,2].zip(a,b)         #=> [[1, 4, 7], [2, 5, 8]]
  #     a.zip([1,2],[8])       #=> [[4,1,8], [5,2,nil], [6,nil,nil]]
  #
  def zip(*others, &block)
    my_size = self.__size
    if block_given?
      others = others.map { |a| a.to_ary }
      i = 0
      while i < my_size
        ary = [ self.__at(i) ]
        j = 0
        other_len = others.length
        while j < other_len
          ary.__push( (others.__at(j)).__at(i) )
          j += 1
        end
        block.call(ary)
        i += 1
      end
      nil
    else
      out = Array.new(my_size)  # not self.class .new
      i = 0
      while i < my_size
	out.__at_put(i, [] )
	i += 1
      end
      others = others.map { |a| a.to_ary }
      i = 0
      while i < my_size
	ary = [ self.__at(i) ]
	j = 0
	other_len = others.length
	while j < other_len
	  ary.__push( (others.__at(j)).__at(i) )
	  j += 1
	end
	# b.call(ary)...
	if block_given?
	  block.call(ary) 
	end
	out.__at_put(i,  ary )
	i += 1
      end
      out
    end
  end


end
