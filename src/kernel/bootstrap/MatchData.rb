class MatchData
  primitive 'at', 'at:'
  primitive_nobridge '[]' , '_rubyAt:'
  primitive '[]' , '_rubyAt:length:'

#   def inspect
#     matches = []
#     i = 0
#     lim = length
#     while i < lim
#       matches << "'#{@inputString[self.begin(i), self.end(i)]}'"
#       i += 1
#     end
#     "MatchData: pre_match='#{pre_match}', [#{matches.join(',')}], post_match='#{post_match}'"
#   end

  def begin(group)
    at((group*2)+1)
  end

  def end(group)
    at((group*2)+2)
  end

  primitive 'size', 'size'
  def length
    size / 2
  end

  def pre_match
    res = @strPreceedingMatch
    if (res.equal?(nil))
      res = @inputString[0..self.begin(0)-1]
      @strPreceedingMatch = res
    end
    res
  end

  def post_match
    res = @strFollowingMatch
    if (res.equal?(nil))
      res = @inputString[self.begin(0)+self[0].size..-1]
      @strFollowingMatch = res
    end
    res
  end

  def _plus_match
    self[ self.length - 1 ]
  end

  def string
    @inputString
  end


  # Ruby global variables $1..$9 implemented by MatchData(C)>>nthRegexRef:

  # BEGIN RUBINIUS

  def collapsing?
    r = self.begin(0) == self.end(0)
    puts "'#{self.string}'.collapsing?: #{r} self.begin(0) #{self.begin(0)} self.end(0) #{self.end(0)}"
    r
  end

  def pre_match_from(idx)
    return "" if self.begin(0) == 0 # GEMSTONE
    pre_end = self.begin(0) - 1
    @inputString[idx, pre_end-idx+1]
  end

  def captures  # GEMSTONE modified loop from each {...} to while
    out = []
    i = 0
    lim = length
    while i < lim
      x = self.begin(i)
      if x == -1
        out << nil
      else
        y = self.end(i)
        out << @inputString[x, y-x]  # GEMSTONE
      end
      i += 1
    end
    return out
  end
  # END RUBINIUS

end

