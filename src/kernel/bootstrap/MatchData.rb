class MatchData
  primitive 'at', 'at:'
  primitive_nobridge '[]' , '_rubyAt:'
  primitive '[]' , '_rubyAt:length:'

  def inspect
    matches = [self[0]].concat(self.captures)
    "MatchData: #{matches.inspect}"
  end

  def to_a
    __captures([self[0]])
  end

  def to_s
    self[0]
  end

  def begin(group)
    at((group*2)+1)
  end

  def values_at(*indicies)
    result = []
    i = 0
    lim = indicies.length
    while i < lim
      result << self[indicies[i]]
      i += 1
    end
    result
  end

  def end(group)
    at((group*2)+2)
  end

  primitive '__size', 'size'
  def size
    __size.__divide(2)
  end

  alias length size

  def pre_match
    # public, and also invoked from generated code for RubyBackRefNode
    res = @strPreceedingMatch
    if (res.equal?(nil))
      res = @inputString[0, self.begin(0)]
      @strPreceedingMatch = res
    end
    res
  end

  def post_match
    # public, and also invoked from generated code for RubyBackRefNode
    res = @strFollowingMatch
    if (res.equal?(nil))
      res = @inputString[self.end(0)..-1]
      @strFollowingMatch = res
    end
    res
  end

  def __plus_match
    # invoked from generated code for RubyBackRefNode
    self[ self.length - 1 ]
  end

  def string
    @inputString
  end


  # Ruby global variables $1..$9 implemented by MatchData(C)>>nthRegexRef:

  # BEGIN RUBINIUS

  def collapsing?
    self.begin(0) == self.end(0)
  end

  def pre_match_from(idx)
    return "" if self.begin(0).equal?(0) # GEMSTONE
    pre_end = self.begin(0) - 1
    @inputString[idx, pre_end-idx+1]
  end

  def captures 
    __captures([])
  end

  # END RUBINIUS

  def offset(n)
    [self.begin(n), self.end(n)]
  end

  def select(&block)
    result = []
    i = 0
    lim = self.size
    while i < lim
      elem = self[i]
      result << elem if block.call( elem )
      i += 1
    end
    result
  end

  # Append the captures (all but $&) to ary
  def __captures(ary)
    i = 1   # Captures do NOT include $& (the entire matched string)
    lim = length
    in_str = @inputString
    while i < lim
      x = self.begin(i)
      if x == -1
        ary << nil
      else
        y = self.end(i)
        ary << in_str[x, y-x]  # GEMSTONE
      end
      i += 1
    end
    ary
  end
end

