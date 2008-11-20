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

  def pbm
    puts self[0].class
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
    return self if size == 0

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

        break if carry == 0
        last_alnum = start
      end

      start -= 1
    end

    if carry.nil?
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


end
