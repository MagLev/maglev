class MatchData
  primitive 'at', 'at:'
  primitive_nobridge '[]' , '_rubyAt:'
  primitive '[]' , '_rubyAt:length:'

  def begin(group)
    at((group*2)+1)
  end

  def end(group)
    at((group*2)+2)
  end

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

  def full; self.at(1); end  # GEMSTONE

  def collapsing?
    self.begin(0) == self.end(0)
  end

  def pre_match_from(idx)
#    return "" if full.at(0) == 0
    return "" if full == 0  # GEMSTONE
    nd = full.at(0) - 1
#    @source[idx, nd-idx+1]
    @inputString[idx, nd-idx+1]   # GEMSTONE
  end

  def captures
    out = []

#  TODO: WIP: finish
#     @region.each do |tup|


#       x = tup.at(0)

#       if x == -1
#         out << nil
#       else
#         y = tup.at(1)
# #        out << @source[x, y-x]
#         out << source[x, y-x]  # GEMSTONE
#       end
#     end
    return out
  end


  # END RUBINIUS

end

