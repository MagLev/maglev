class Range
  primitive 'hash'
  primitive 'length', 'size'

  def begin
    @from
  end
  def first
    @from
  end

  def end
    @to
  end
  def last
    @to
  end

  def ===(n)
    return false if n < @from
    return false if n > @to
    if @excludeEnd
      return false if n == @to
    end
    return true
  end

  def each
          # adapted from fix contributed by Markus
    x = @from
    llast = @to
    if x._isFixnum
      if @excludeEnd
        while x < llast
          yield x
          x = x + 1
        end
      else
        while x <= llast
          yield x
          x = x + 1
        end
      end
    else
      raise TypeError, "can't iterate from #{x.class}" unless x.respond_to?(:succ)
      if llast._isString
        sz = llast.size
        if @excludeEnd
          while (x < llast) and (x.size <= sz)
            yield x
            x = x.succ
          end
        else
          while (x <= llast) and (x.size <= sz)
            yield x
            x = x.succ
          end
        end
      elsif x._isNumeric && llast._isNumeric
        if @excludeEnd
          while x < llast
            yield x
            x = x.succ
          end
        else
          while x <= llast
            yield x
            x = x.succ
          end
        end
      else
        if @excludeEnd
          while (x <=> llast) < 0
            yield x
            x = x.succ
          end
        else
          while (x <=> llast) <= 0
            yield x
            if (x <=> llast) == 0
              break   # per ruby specs; llast.succ might return llast 
            end
            x = x.succ
          end
        end
      end
   end
  end

  primitive 'end', '_to'

  def eql?(other)
    other._isRange &&
      @from.eql?(other.first) &&
      @to.eql?(other.last) &&
      @excludeEnd.eql?(other.exclude_end?)
  end

  alias == eql?

  def exclude_end?
    @excludeEnd
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

    @from = fromArg
    @to = toArg
    @by = 1
    @excludeEnd = exclusive
  end

  # Convert this range object to a printable form (using
  # <tt>inspect</tt> to convert the start and end objects).
  def inspect
    ts = Thread._recursion_guard_set
    added = ts._add_if_absent(self)
    unless added
      return #{@excludeEnd ? "..." : ".."}
    end
    begin
      s = "#{@from.inspect}#{@excludeEnd ? "..." : ".."}#{@to.inspect}"
    ensure
      ts.remove(self)
    end
    s
  end

  def step(n=1, &block)
    current = @from
    lim = @to
    if @from._isNumeric
      unless n._isNumeric
        n = Type.coerce_to(n, Integer, :to_int)
      end
      if (n <= 0)
        raise ArgumentError, 'increment for step must be > 0'
      end
      if @excludeEnd
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
      unless @excludeEnd
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
    "#{@from}#{@excludeEnd ? "..." : ".."}#{@to}"
  end

  # We don't use the smalltalk asArray method, since it uses '+' rather
  # than 'succ' to get the next element of the range.
  def to_a
    result = []
    self.each { |e| result << e }  # each will use succ
    result
  end

  # Given a target length, +len+, Calculate whether this range covers the given length.
  # If it does, return the beginning and length of the "string" of length +len+.
  # Returns nil if the range does not cover.  See rb_range_beg_len.
  # This does the appropriate Type.coerce_to that the specs expect.
  # +err+ is ignored for now.
  def _beg_len(len, err=0)
    beg = Type.coerce_to(@from, Fixnum, :to_int)
    the_end = Type.coerce_to(@to, Fixnum, :to_int)

    if (beg < 0)
      beg += len
      return nil if (beg < 0)
    end

    if (err == 0 || err == 2)
      return nil if (beg > len)
      the_end = len if (the_end > len)
    end

    the_end += len if (the_end < 0)
    the_end += 1 unless @excludeEnd
    len = the_end - beg
    len = 0 if (len < 0)
    return [beg, len]
  end
end
