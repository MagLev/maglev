class Range
  FlipFlop = __resolve_smalltalk_global(:RubyFlipFlop) # code in FlipFlop.rb

  primitive 'hash'

  def begin
    @_st_from
  end
  def first
    @_st_from
  end

  def end
    @_st_to
  end
  def last
    @_st_to
  end

  def ===(n)
    from_cmp = @_st_from <=> n 
    if from_cmp._equal?(nil) || from_cmp > 0
      return false
    end
    to_cmp =   @_st_to <=> n
    return false if to_cmp < 0
    if @_st_excludeEnd
      return false if to_cmp == 0
    end
    return true
  end

  def each(&block)
    unless block_given?
      return RangeEnumerator.new(self, :each, 1) # for 1.8.7
    end
          # adapted from fix contributed by Markus
    x = @_st_from
    llast = @_st_to
    if x._isFixnum
      if @_st_excludeEnd
        while x < llast
          block.call( x )
          x = x + 1
        end
      else
        while x <= llast
          block.call( x )
          x = x + 1
        end
      end
    else
      raise TypeError, "can't iterate from #{x.class}" unless x.respond_to?(:succ)
      if llast._isString
        sz = llast.size
        if @_st_excludeEnd
          while (x < llast) and (x.size <= sz)
            block.call( x )
            x = x.succ
          end
        else
          while (x <= llast) and (x.size <= sz)
            block.call( x )
            x = x.succ
          end
        end
      elsif x._isNumeric && llast._isNumeric
        if @_st_excludeEnd
          while x < llast
            block.call( x )
            x = x.succ
          end
        else
          while x <= llast
            block.call( x )
            x = x.succ
          end
        end
      else
        if @_st_excludeEnd
          while (x <=> llast) < 0
            block.call( x )
            x = x.succ
          end
        else
          while (x <=> llast) <= 0
            block.call( x )
            if (x <=> llast) == 0
              break   # per ruby specs; llast.succ might return llast 
            end
            x = x.succ
          end
        end
      end
    end
    self   # change for 1.8.7
  end

  def eql?(other)
    other._isRange &&
      @_st_from.eql?(other.first) &&
      @_st_to.eql?(other.last) &&
      @_st_excludeEnd.eql?(other.exclude_end?)
  end

  alias == eql?

  def exclude_end?
    @_st_excludeEnd
  end

  def __limit
    if @_st_excludeEnd
      @_st_to
    else
      @_st_to + @_st_by
    end
  end

  def length
    ((self.__limit - @_st_from).__divide( @_st_by )).__max(0) 
  end

  def initialize(fromArg, toArg, exclusive=false)
    unless fromArg._isFixnum && toArg._isFixnum
      begin
        raise ArgumentError, "bad value for range" unless fromArg <=> toArg
      rescue
        # In case <=> isn't defined, we also need to raise an error
        raise ArgumentError, "bad value for range"
      end
    end

    @_st_from = fromArg
    @_st_to = toArg
    @_st_by = 1
    @_st_excludeEnd = exclusive
  end

  # Convert this range object to a printable form (using
  # <tt>inspect</tt> to convert the start and end objects).
  def inspect
    ts = Thread.__recursion_guard_set
    added = ts.__add_if_absent(self)
    unless added
      return #{@_st_excludeEnd ? "..." : ".."}
    end
    begin
      s = "#{@_st_from.inspect}#{@_st_excludeEnd ? "..." : ".."}#{@_st_to.inspect}"
    ensure
      ts.remove(self)
    end
    s
  end

  def step(n=1, &block)
    # algorithm replicated in RangeEnumerator
    current = @_st_from
    lim = @_st_to
    if @_st_from._isNumeric
      unless n._isNumeric
        n = Maglev::Type.coerce_to(n, Integer, :to_int)
      end
      if (n <= 0)
        raise ArgumentError, 'increment for step must be > 0'
      end
      unless block_given?
        return RangeEnumerator.new(self, :step, n)  # for 1.8.7
      end
      if @_st_excludeEnd
        begin
          block.call(current)
          current += n
        end while current < lim
      else
        begin
          block.call(current)
          current += n
        end while current <= lim
      end
    else
      unless block_given?
        return RangeEnumerator.new(self, :step, n)  # for 1.8.7
      end
      unless @_st_excludeEnd
        lim = lim.succ
      end
      begin
        block.call(current)
        n.times { |i| current = current.succ }
      end while current < lim
    end
    self
  end

  def to_s
    "#{@_st_from}#{@_st_excludeEnd ? "..." : ".."}#{@_st_to}"
  end

  # We don't use the smalltalk asArray method, since it uses '+' rather
  # than 'succ' to get the next element of the range.
  def to_a
    result = []
    self.each { |e| result << e }  # each will use succ
    result
  end

  def __integer_to_a
    # optimized implementation of to_a 
    # which assumes receiver is a simple range with @_st_from <= @_st_to
    n = @_st_from
    last = @_st_to
    unless n._isFixnum && last._isFixnum 
      raise TypeError, 'Range is not over Fixnums'
    end
    if @_st_excludeEnd
      last -= 1
    end
    count = (last - n).__max(0)
    res = Array.new(count)
    idx = 0
    while n <= last
      res[idx] = n
      n += 1
      idx += 1
    end
    res
  end

  # Given a target length, +len+, Calculate whether this range covers the given length.
  # If it does, return the beginning and length of the "string" of length +len+.
  # Returns nil if the range does not cover.  
  # This does the appropriate Maglev::Type.coerce_to that the specs expect.
  # +err+ is ignored for now.
  def __beg_len(len)
    beg = Maglev::Type.coerce_to(@_st_from, Fixnum, :to_int)
    the_end = Maglev::Type.coerce_to(@_st_to, Fixnum, :to_int)

    if (beg < 0)
      beg += len
      return nil if (beg < 0)
    end

    return nil if (beg > len)
    the_end = len if (the_end > len)

    the_end += len if (the_end < 0)
    the_end += 1 unless @_st_excludeEnd
    len = the_end - beg
    len = 0 if (len < 0)
    return [beg, len]
  end

  def __cext_beg_len(len, err)
    beg = Maglev::Type.coerce_to(@_st_from, Fixnum, :to_int)
    the_end = Maglev::Type.coerce_to(@_st_to, Fixnum, :to_int)
    if (beg < 0)
      beg += len
      return nil if (beg < 0)
    end
    if (err == 0 || err == 2) 
      return nil if (beg > len)
      if (the_end > len) ; the_end = len ; end
    end
    if (the_end < 0) ; the_end += len ; end
    the_end += 1 unless @_st_excludeEnd
    len = the_end - beg 
    if (len < 0) ; len = 0 ; end
   
    [ beg, len ]
  end

end
Range.__freeze_constants
