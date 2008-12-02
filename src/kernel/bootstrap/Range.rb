class Range
  primitive 'hash'
  primitive 'length', 'size'

  def ===(n)
    return false if n < first
    return false if n > last
    if exclude_end?
      return false if n == last
    end
    return true
  end

  primitive 'begin', '_from'

  def each(&block)
    raise TypeError, "can't iterate from #{first.class}" unless first.respond_to? :succ

    current = @from
    limit = exclude_end? ? @to : @to.succ
    while (current < limit)
      block.call(current)
      nxt = current.succ
      if (nxt.size > current.size)
        return  # special semantics for Ranges of Strings
      end
      current = nxt
    end
  end

  primitive 'end', '_to'

  def eql?(other)
    other.is_a?(Range) &&
      self.first.eql?(other.first) &&
      self.last.eql?(other.last) &&
      self.exclude_end?.eql?(other.exclude_end?)
  end

  alias == eql?

  primitive 'exclude_end?', 'excludeEnd'
  primitive 'first', '_from'

  def initialize(fromArg, toArg, exclusive=false)
    unless fromArg.is_a?(Fixnum) && toArg.is_a?(Fixnum)
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
      touchedSet = Set.new
    else
      if (touchedSet._includes(self))
        return #{@excludeEnd ? "..." : ".."}
      end
    end
    touchedSet << self
    "#{@from.inspect(touchedSet)}#{@excludeEnd ? "..." : ".."}#{@to.inspect(touchedSet)}"
  end

  primitive 'last', '_to'

  def step(n=1, &block)
    is_numeric_range = @from.is_a?(Numeric)
    current = @from
    begin
      block.call(current)
      if is_numeric_range
        current += n
      else
        n.times { current = current.succ }
      end
    end while current < @to
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
