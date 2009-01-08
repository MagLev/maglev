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
    other._isRange &&
      self.first.eql?(other.first) &&
      self.last.eql?(other.last) &&
      self.exclude_end?.eql?(other.exclude_end?)
  end

  alias == eql?

  primitive 'exclude_end?', 'excludeEnd'
  primitive 'first', '_from'

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
