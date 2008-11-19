class Range
  primitive 'to_a', 'asArray'
  primitive '==', '='
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
  #  primitive 'each&', 'do:'
  def each(&block)
    raise TypeError, "can't iterate from #{first.class}" unless first.respond_to? :succ

    current = @from
    limit = exclude_end? ? @to : @to.succ
    begin
      block.call(current)
      current = current.succ
    end while current < limit
  end

  primitive 'end', '_to'

  def eql?(other)
    other.is_a?(Range) &&
      self.first.eql?(other.first) &&
      self.last.eql?(other.last) &&
      self.exclude_end?.eql?(other.exclude_end?)
  end

  primitive 'exclude_end?', 'excludeEnd'
  primitive 'first', '_from'

  def initialize(fromArg, toArg)
    @from = fromArg
    @to = toArg
    @by = 1
    @excludeEnd = false
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
end
