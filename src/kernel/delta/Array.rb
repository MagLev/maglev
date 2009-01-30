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
    select{|ea| pattern === ea}
  end

  # TODO: include?: The Pick Axe book documents include? under both Array
  # and Enumerable. Our implementation is in the Array section above.

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
      # b.call(ary)...
        yield(ary) if block_given?
      result << ary
      i += 1
    end
    result
  end
end
