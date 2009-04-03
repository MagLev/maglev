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
    lim = size
    if block_given?
      while n < lim
        unless yield(self[n]) ; return false ; end
        n = n + 1
      end
    else
      while n < lim
        unless self[n] ; return false ; end
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
  def reject!(&b)
    a = reject(&b)
    if a.size == size
      nil # no changes, return nil
    else
      replace(a)
      self
    end
  end

  def any?(&block)
    n = 0
    lim = size
    if block_given?
      while n < lim
        if yield(self[n]) ; return true ; end
        n = n + 1
      end
    else
      while n < lim
        if self[n] ; return true ; end
        n = n + 1
      end
    end
    false
  end

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

  def detect(ifnone_proc=nil, &block)
    n = 0
    lim = size
    while n < lim
      elem = self[n]
      if yield(elem)
        return elem
      end
      n = n + 1
    end
    if ifnone_proc.equal?(nil)
      nil
    else
      ifnone_proc.call
    end
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

  alias find  detect

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
    result = []
    i = 0
    lim = size
    while i < lim
      result << self[i] if pattern === self[i]
      i += 1
    end
    result
  end

  def inject(&blk)
    my_size = size
    accum = nil
    n = 0
    if my_size > 0
      accum = self[0]
      n = 1
      while (n < my_size)
        accum = yield(accum, self[n])
        n = n + 1
      end
    end
    accum
  end

  def inject(initial, &blk)
    accum = initial
    my_size = size
    n = 0
    while (n < my_size)
      accum = yield(accum, self[n])
      n = n + 1
    end
    accum
  end

  alias map collect

  def max(&blk)
    return nil if size.equal?(0)

    max_v = self[0]
    i = 1
    lim = size
    if block_given?
      while i < lim
        o = self[i]
        comp = blk.call(o, max_v)
        raise ArgumentError, "comparison of #{o.class} with #{max_v} failed" if comp.nil?
        max_v = o if comp > 0
        i += 1
      end
    else
      while i < lim
        o = self[i]
        max_v = o if (o <=> max_v) > 0
        i += 1
      end
    end
    max_v
  end

  primitive 'member?', 'includes:'

  def min(&blk)
    return nil if size.equal?(0)

    min_v = self[0]
    i = 1
    lim = size
    if block_given?
      while i < lim
        o = self[i]
        comp = blk.call(o, min_v)
        raise ArgumentError, "comparison of #{o.class} with #{min_v} failed" if comp.nil?
        min_v = o if comp < 0
        i += 1
      end
    else
      while i < lim
        o = self[i]
        min_v = o if (o <=> min_v) < 0
        i += 1
      end
    end
    min_v
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

  def select(&b)
    result = []
    i = 0
    lim = size
    while i < lim
      result << self[i] if b.call(self[i])
      i += 1
    end
    result
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
  def zip(*others)
    out = Array.new(size) { [] }
    others = others.map { |a| a.to_ary }

    i = 0
    lim = size
    while i < lim
      ary = [self[i]]

      j = 0
      while j < others.length
        ary << others[j][i]
        j += 1
      end
      # b.call(ary)...
      yield(ary) if block_given?
      out[i] = ary
      i += 1
    end
    return block_given? ? nil : out
  end


  # Generates a string from converting all elements of
  # the Array to strings, inserting a separator between
  # each. The separator defaults to $,. Detects recursive
  # Arrays.
  def join(s = Undefined)
    # this is mostly Rubinius code, but modified for our API
    # Put in delta since we need the recursion guard found in common.
    my_size = size
    return "" if my_size.equal?(0)
    if s.equal?(Undefined)
      sep = $,
    else
      begin
        sep = s.nil? ? nil : s.to_str
      rescue NoMethodError
        raise TypeError, "Cannot convert #{s.inspect} to str"
      end
    end

    out = ""
#    out.taint if sep.tainted? or self.tainted?
    i = 0
    while(i < my_size)
      elem = at(i)
      out << sep unless (sep.equal?(nil) || i == 0)

      if elem.kind_of?(Array)
        if RecursionGuard.inspecting?(elem)
          out << "[...]"
        else
          RecursionGuard.inspect(self) do
            out << elem.join(sep)
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

end
