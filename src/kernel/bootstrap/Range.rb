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
    if exclude_end?
      return false if n == @to
    end
    return true
  end

  def each
   				# adapted from fix contributed by Markus
    x = @from
    raise TypeError, "can't iterate from #{x.class}" unless x.respond_to?(:succ)
    llast = @to
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
    else
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
  def inspect(touchedSet=nil)
    if (touchedSet.equal?(nil))
      touchedSet = IdentitySet.new
    else
      if (touchedSet._includes(self))
        return #{@excludeEnd ? "..." : ".."}
      end
    end
    touchedSet << self
    "#{@from.inspect(touchedSet)}#{@excludeEnd ? "..." : ".."}#{@to.inspect(touchedSet)}"
  end

  def step(n=1, &block)
    current = @from
    if @from._isNumber
      begin
        block.call(current)
        current += n
      end while current < @to
    else
      begin
        block.call(current)
        n.times { |i| current = current.succ }
      end while current < @to
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
end
