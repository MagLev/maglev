class String
  primitive '_splice', 'copyReplaceFrom:to:with:'

  # This is a rubinius helper function
  def splice!(start, count, replacement)
    replace _splice(start+1, start+count, replacement)
  end

  # Returns the successor to <i>self</i>. The successor is calculated by
  # incrementing characters starting from the rightmost alphanumeric (or
  # the rightmost character if there are no alphanumerics) in the
  # string. Incrementing a digit always results in another digit, and
  # incrementing a letter results in another letter of the same case.
  # Incrementing nonalphanumerics uses the underlying character set's
  # collating sequence.
  #
  # If the increment generates a ``carry,'' the character to the left of
  # it is incremented. This process repeats until there is no carry,
  # adding an additional character if necessary.
  #
  #   "abcd".succ        #=> "abce"
  #   "THX1138".succ     #=> "THX1139"
  #   "<<koala>>".succ   #=> "<<koalb>>"
  #   "1999zzz".succ     #=> "2000aaa"
  #   "ZZZ9999".succ     #=> "AAAA0000"
  #   "***".succ         #=> "**+"
  def succ
    self.dup.succ!
  end

  def upto(stop)
    #stop = StringValue(stop)
    stop = Type.coerce_to(stop, String, :to_str)

    return self if self > stop

    after_stop = stop.succ
    current = self

    until current == after_stop
      yield current
      #current = StringValue(current.succ)
      current = Type.coerce_to(current.succ, String, :to_str)
      break if current.size > stop.size || current.size == 0
    end

    self
  end

  def self.isalnum(int)
    (48..57).include?(int) ||
      (65..90).include?(int) ||
      (97..122).include?(int)
  end
  # Equivalent to <code>String#succ</code>, but modifies the receiver in
  # place.
  def succ!
#    return self if @num_bytes == 0
    return self if size.equal?(0)

    carry = nil
    last_alnum = 0
#    start = @num_bytes - 1
    start = size - 1

#    self.modify!

    while start >= 0
#      if (s = @data[start]).isalnum
      #      if (s = self[start]).isalnum
      s = self[start]
      if String.isalnum(s)
        carry = 0
        if (?0 <= s && s < ?9) ||
           (?a <= s && s < ?z) ||
           (?A <= s && s < ?Z)
#          @data[start] += 1
          self[start] += 1
        elsif s == ?9
#          @data[start] = ?0
          self[start] = ?0
          carry = ?1
        elsif s == ?z
#          @data[start] = carry = ?a
          self[start] = carry = ?a
        elsif s == ?Z
#          @data[start] = carry = ?A
          self[start] = carry = ?A
        end

        break if carry.equal?(0)
        last_alnum = start
      end

      start -= 1
    end

    if carry.equal?(nil)
      start = length - 1
      carry = ?\001

      while start >= 0
#         if @data[start] >= 255
#           @data[start] = 0
#         else
#           @data[start] += 1
#           break
#         end
        if self[start] >= 255
          self[start] = 0
        else
          self[start] += 1
          break
        end

        start -= 1
      end
    end

    if start < 0
#      splice! last_alnum, 1, carry.chr + @data[last_alnum].chr
      splice! last_alnum, 1, carry.chr + self[last_alnum].chr
    end

    return self
  end

  alias_method :next, :succ
  alias_method :next!, :succ!

  # Both forms iterate through <i>self</i>, matching the pattern (which may be a
  # <code>Regexp</code> or a <code>String</code>). For each match, a result is
  # generated and either added to the result array or passed to the block. If
  # the pattern contains no groups, each individual result consists of the
  # matched string, <code>$&</code>.  If the pattern contains groups, each
  # individual result is itself an array containing one entry per group.
  #
  #   a = "cruel world"
  #   a.scan(/\w+/)        #=> ["cruel", "world"]
  #   a.scan(/.../)        #=> ["cru", "el ", "wor"]
  #   a.scan(/(...)/)      #=> [["cru"], ["el "], ["wor"]]
  #   a.scan(/(..)(..)/)   #=> [["cr", "ue"], ["l ", "wo"]]
  #
  # And the block form:
  #
  #   a.scan(/\w+/) {|w| print "<<#{w}>> " }
  #   print "\n"
  #   a.scan(/(.)(.)/) {|x,y| print y, x }
  #   print "\n"
  #
  # <em>produces:</em>
  #
  #   <<cruel>> <<world>>
  #   rceu lowlr

  def scan(pattern)
    taint = self.tainted? || pattern.tainted?
    pattern = get_pattern(pattern, true)
    index = 0
    last_match = nil
    ret = []

    while match = pattern.match_from(self, index)
      index = match.collapsing? ? match.end(0) + 1 : match.end(0)
      last_match = match
      val = (match.length.equal?(1) ? match[0] : match.captures)
      val.taint if taint
      ret << val
    end

    last_match._storeRubyVcGlobal(0x20) # store into caller's $~
    return ret
  end

  def scan(pattern, &block)
    # second variant gets no bridge methods. can't rely on single
    # implementation with bridge methods when using _storeRubyVcGlobal
    # because number of frames up to caller's frame would vary .
    taint = self.tainted? || pattern.tainted?
    pattern = get_pattern(pattern, true)
    index = 0
    last_match = nil
    if block_given?
      ret = self
      while match = pattern.match_from(self, index)
        index = match.collapsing? ? match.end(0) + 1 : match.end(0)
        last_match = match
        val = (match.length.equal?(1) ? match[0] : match.captures)
        val.taint if taint

        last_match._storeRubyVcGlobal(0x20) # store into caller's $~
        yield(val)
      end
    else
      ret = []
      while match = pattern.match_from(self, index)
        index = match.collapsing? ? match.end(0) + 1 : match.end(0)
        last_match = match
        val = (match.length.equal?(1) ? match[0] : match.captures)
        val.taint if taint

        ret << val
      end
    end

    last_match._storeRubyVcGlobal(0x20) # store into caller's $~
    return ret
  end

end
