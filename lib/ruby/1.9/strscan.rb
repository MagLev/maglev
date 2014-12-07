# -*- coding: utf-8 -*-
# original copy from Rubinius  March 09
#  modified to be Maglev style efficient ruby code.
#  see file  strscan.rb.base  for the original Rubinius code

class ScanError < StandardError; end

class StringScanner
  Id = "bite me $Id".freeze
  Version = "1.0.0".freeze

  # attr_reader :pos, :match, :string

  # Returns the position of the scan pointer. In the <em>reset</tm>
  # position, this value is zero. In the <em>terminated</em> position
  # (i.e. the string is exhausted), this value is the length of the string.
  #
  # In short, it's a 0-based index into the string.
  #
  #   s = StringScanner.new('test string')
  #   s.pos               # -> 0
  #   s.scan_until /str/  # -> "test str"
  #   s.pos               # -> 8
  #   s.terminate         # -> #<StringScanner fin>
  #   s.pos               # -> 11
  def pos
    @pos
  end

  def match
    @match
  end

  # Modify the scan pointer.
  #
  #   s = StringScanner.new('test string')
  #   s.pos = 7            # -> 7
  #   s.rest               # -> "ring"
  def pos=(n)
    unless n._isFixnum
      n = Maglev::Type.coerce_to(n, Fixnum, :to_int)
    end
    raise RangeError, "xxx" if n > @string.size
    @pos = n
  end

  alias :pointer :pos
  alias :pointer= :pos=

  # Return the n-th subgroup in the most recent match.
  #
  #   s = StringScanner.new("Fri Dec 12 1975 14:39")
  #   s.scan(/(\w+) (\w+) (\d+) /)       # -> "Fri Dec 12 "
  #   s[0]                               # -> "Fri Dec 12 "
  #   s[1]                               # -> "Fri"
  #   s[2]                               # -> "Dec"
  #   s[3]                               # -> "12"
  #   s.post_match                       # -> "1975 14:39"
  #   s.pre_match                        # -> ""
  def [](n)
    # coercion will be implicit by match[] primitive
    unless n._isFixnum
      n = Maglev::Type.coerce_to(n, Fixnum, :to_int)
    end
    begin
      m = self.match
      unless match._equal?(nil)
        m = m[n]
      end
      m
    rescue IndexError
      nil
    end
  end

  # Returns true iff the scan pointer is at the beginning of the line.
  #
  #   s = StringScanner.new("test\ntest\n")
  #   s.bol?           # => true
  #   s.scan(/te/)
  #   s.bol?           # => false
  #   s.scan(/st\n/)
  #   s.bol?           # => true
  #   s.terminate
  #   s.bol?           # => true
  def bol?
    if pos.equal?(0)
      return true
    end
    pminus = pos - 1
    string[pminus..pminus] == "\n"
  end
  alias :beginning_of_line? :bol?

  # This returns the value that scan would return, without advancing the
  # scan pointer. The match register is affected, though.
  #
  #   s = StringScanner.new("Fri Dec 12 1975 14:39")
  #   s.check /Fri/               # -> "Fri"
  #   s.pos                       # -> 0
  #   s.matched                   # -> "Fri"
  #   s.check /12/                # -> nil
  #   s.matched                   # -> nil
  #
  # Mnemonic: it "checks" to see whether a scan will return a value.
  #
  def check(pattern)
    _scan_headonly(pattern, false, true)
  end

  # This returns the value that scan_until would return, without advancing
  # the scan pointer. The match register is affected, though.
  #
  #   s = StringScanner.new("Fri Dec 12 1975 14:39")
  #   s.check_until /12/          # -> "Fri Dec 12"
  #   s.pos                       # -> 0
  #   s.matched                   # -> 12
  #
  # Mnemonic: it "checks" to see whether a scan_until will return a value.
  #
  def check_until(pattern)
    _scan(pattern, false, true)
  end

  # Equivalent to #terminate. This method is obsolete; use #terminate instead.
  def clear
    warn "StringScanner#clear is obsolete; use #terminate instead" if $VERBOSE
    terminate
  end

  # Appends +str+ to the string being scanned. This method does not affect
  # scan pointer.
  #
  #   s = StringScanner.new("Fri Dec 12 1975 14:39")
  #   s.scan(/Fri /)
  #   s << " +1000 GMT"
  #   s.string            # -> "Fri Dec 12 1975 14:39 +1000 GMT"
  #   s.scan(/Dec/)       # -> "Dec"
  #
  def concat(str)
    @string << str
    self
  end
  alias :<< :concat # TODO: reverse

  # Equivalent to #eos?. This method is obsolete, use #eos? instead.
  def empty?
    warn "StringScanner#empty? is obsolete; use #eos? instead?" if $VERBOSE
    eos?
  end

  # Returns true if the scan pointer is at the end of the string.
  #
  #   s = StringScanner.new('test string')
  #   p s.eos?          # => false
  #   s.scan(/test/)
  #   p s.eos?          # => false
  #   s.terminate
  #   p s.eos?          # => true
  def eos?
    @pos >= @string.size
  end

  # Looks ahead to see if the pattern exists anywhere in the string,
  # without advancing the scan pointer. This predicates whether a
  # #scan_until will return a value.
  #
  #   s = StringScanner.new('test string')
  #   s.exist? /s/            # -> 3
  #   s.scan /test/           # -> "test"
  #   s.exist? /s/            # -> 6
  #   s.exist? /e/            # -> nil
  def exist?(pattern)
    p = _scan(pattern, false, false)
    if p._equal?(nil)
      p
    else
      @pos > 0 ? p -1 : p
    end
  end

  # Scans one byte and returns it. This method is NOT multi-byte character
  # sensitive. See also getch.
  #
  #   s = StringScanner.new('ab')
  #   s.get_byte         # => "a"
  #   s.get_byte         # => "b"
  #   s.get_byte         # => nil
  #
  #   s = StringScanner.new("\244\242")
  #   s.get_byte         # => "\244"
  #   s.get_byte         # => "\242"
  #   s.get_byte         # => nil
  def get_byte
    scan(/./mn)
  end

  # Equivalent to #get_byte. This method is obsolete; use #get_byte instead.
  def getbyte
    warn "StringScanner#getbyte is obsolete; use #get_byte instead" if $VERBOSE
    get_byte
  end

  # Scans one character and returns it. This method is multi-byte character
  # sensitive. See also #get_byte.
  #
  #   s = StringScanner.new('ab')
  #   s.getch           # => "a"
  #   s.getch           # => "b"
  #   s.getch           # => nil
  #
  #   $KCODE = 'EUC'
  #   s = StringScanner.new("\244\242")
  #   s.getch           # => "\244\242"   # Japanese hira-kana "A" in EUC-JP
  #   s.getch           # => nil
  def getch
    scan(/./m)
  end

  # Creates a new StringScanner object to scan over the given
  # +string+. +dup+ argument is obsolete and not used now.
  def initialize(string, dup = false)
    unless string._isString
      string = Maglev::Type.coerce_to(string, String, :to_str)
    end
    @string = string
    self.reset
  end

  def initialize_copy(orig)
    @match  = orig.match
    @pos    = orig.pos
    s = orig.string
    @string = s
  end

  # Returns a string that represents the StringScanner object, showing:
  # <ul>
  #   <li>the current position</li>
  #   <li>the size of the string</li>
  #   <li>the characters surrounding the scan pointer</li>
  # </ul>
  #
  #   s = StringScanner.new("Fri Dec 12 1975 14:39") s.inspect # -> ’#<StringScanner 0/21 @ "Fri D…">’ s.scan_until /12/ # -> "Fri Dec 12" s.inspect # -> ’#<StringScanner 10/21 "…ec 12" @ " 1975…">’
  #
  def inspect
    if defined? @string then
      rest = string.size > 5 ? string[pos..pos+4] + "..." : string
      r = if eos? then
            "#<StringScanner fin>"
          elsif pos > 0 then
            prev = string[0...pos].inspect
            "#<StringScanner #{pos}/#{string.size} #{prev} @ #{rest.inspect}>"
          else
            "#<StringScanner #{pos}/#{string.size} @ #{rest.inspect}>"
          end
      r.taint if self.string.tainted?
      r
    else
      "#<StringScanner (uninitialized)>"
    end
  end

  # Tests whether the given pattern is #matched from the current scan
  # pointer. Returns the length of the match, or +nil+. The scan pointer is
  # not advanced.
  #
  #   s = StringScanner.new('test string')
  #   p s.match?(/\w+/)   # -> 4
  #   p s.match?(/\w+/)   # -> 4
  #   p s.match?(/\s+/)   # -> nil
  def match?(pattern)
    _scan_headonly(pattern, false, false)
  end

  # Returns the last matched string.
  #
  #   s = StringScanner.new('test string')
  #   s.match?(/\w+/)     # -> 4
  #   s.matched           # -> "test"
  def matched
    match.to_s if matched?
  end

  # Returns +true+ iff the last match was successful.
  #
  #   s = StringScanner.new('test string')
  #   s.match?(/\w+/)     # => 4
  #   s.matched?          # => true
  #   s.match?(/\d+/)     # => nil
  #   s.matched?          # => false
  def matched?
    not @match.equal?(nil)
  end

  # Returns the size of the most recent match (see #matched), or +nil+ if
  # there was no recent match.
  #
  #   s = StringScanner.new('test string')
  #   s.check /\w+/           # -> "test"
  #   s.matched_size          # -> 4
  #   s.check /\d+/           # -> nil
  #   s.matched_size          # -> nil
  def matched_size
    m = @match
    m.to_s.size if (not m.equal?(nil))
  end

  # Equivalent to #matched_size. This method is obsolete; use #matched_size
  # instead.
  def matchedsize
    warn "StringScanner#matchedsize is obsolete; use #matched_size instead" if $VERBOSE
    matched_size
  end

  # Return the post-match (in the regular expression sense) of the last
  # #scan.
  #
  #   s = StringScanner.new('test string')
  #   s.scan(/\w+/)           # -> "test"
  #   s.scan(/\s+/)           # -> " "
  #   s.pre_match             # -> "test"
  #   s.post_match            # -> "string"
  def post_match
    m = @match
    m.post_match if (not m.equal?(nil))
  end

  # Return the pre-match (in the regular expression sense) of the last
  # #scan.
  #
  #   s = StringScanner.new('test string')
  #   s.scan(/\w+/)           # -> "test"
  #   s.scan(/\s+/)           # -> " "
  #   s.pre_match             # -> "test"
  #   s.post_match            # -> "string"
  def pre_match
    m = @match
    @string[0...(@pos - m.to_s.size)] if (not m.equal?(nil))
  end

  # Reset the scan pointer (index 0) and clear matching data.
  def reset
    @prev_pos = @pos = 0
    @match = nil
    self
  end

  # Returns the "rest" of the string (i.e. everything after the scan
  # pointer). If there is no more data (eos? = true), it returns
  # <tt>""</tt>.
  def rest
    @string[@pos..-1]
  end

  # Returns true iff there is more data in the string. See eos?. This
  # method is obsolete; use eos? instead.
  #
  #   s = StringScanner.new('test string')
  #   s.eos?              # These two
  #   s.rest?             # are opposites.
  def rest?
    @pos < @string.size
  end

  # s.rest_size is equivalent to s.rest.size.
  def rest_size
    p = @pos
    ss = @string.size
    if p < ss
      ss - p
    else
      0
    end
  end

  # s.restsize is equivalent to s.rest_size. This method is obsolete; use
  # #rest_size instead.
  def restsize
    warn "StringScanner#restsize is obsolete; use #rest_size instead" if $VERBOSE
    rest_size
  end

  # Tries to match with pattern at the current position. If there's a
  # match, the scanner advances the "scan pointer" and returns the matched
  # string. Otherwise, the scanner returns nil.
  #
  #   s = StringScanner.new('test string')
  #   p s.scan(/\w+/)   # -> "test"
  #   p s.scan(/\w+/)   # -> nil
  #   p s.scan(/\s+/)   # -> " "
  #   p s.scan(/\w+/)   # -> "string"
  #   p s.scan(/./)     # -> nil
  def scan(pattern)
    _scan_headonly(pattern, true, true)
  end

  # Scans the string until the pattern is matched. Returns the substring up
  # to and including the end of the match, advancing the scan pointer to
  # that location. If there is no match, nil is returned.
  #
  #   s = StringScanner.new("Fri Dec 12 1975 14:39")
  #   s.scan_until(/1/)        # -> "Fri Dec 1"
  #   s.pre_match              # -> "Fri Dec "
  #   s.scan_until(/XYZ/)      # -> nil
  def scan_until(pattern)
    _scan(pattern, true, true)
  end

  # Tests whether the given pattern is matched from the current scan
  # pointer. Returns the matched string if return_string_p is
  # true. Advances the scan pointer if advance_pointer_p is true. The match
  # register is affected.
  #
  # "full" means "scan with full parameters".
  def scan_full(pattern, succptr, getstr)
    _scan_headonly(pattern, succptr, getstr)
  end

  # Scans the string until the pattern is matched. Returns the matched
  # string if return_string_p is true, otherwise returns the number of
  # bytes advanced. Advances the scan pointer if advance_pointer_p,
  # otherwise not. This method does affect the match register.
  def search_full(pattern, succptr, getstr)
    _scan(pattern, succptr, getstr)
  end

  def self.must_C_version
    self
  end

  # Attempts to skip over the given pattern beginning with the scan
  # pointer. If it matches, the scan pointer is advanced to the end of the
  # match, and the length of the match is returned. Otherwise, nil is
  # returned.
  #
  # It's similar to scan, but without returning the matched string.
  #
  #   s = StringScanner.new('test string')
  #   p s.skip(/\w+/)   # -> 4
  #   p s.skip(/\w+/)   # -> nil
  #   p s.skip(/\s+/)   # -> 1
  #   p s.skip(/\w+/)   # -> 6
  #   p s.skip(/./)     # -> nil
  def skip(pattern)
    _scan_headonly(pattern, true, false)
  end

  # Advances the scan pointer until pattern is matched and
  # consumed. Returns the number of bytes advanced, or nil if no match was
  # found.
  #
  # Look ahead to match pattern, and advance the scan pointer to the end of
  # the match. Return the number of characters advanced, or nil if the
  # match was unsuccessful.
  #
  # It's similar to scan_until, but without returning the intervening
  # string.
  #
  #   s = StringScanner.new("Fri Dec 12 1975 14:39")
  #   s.skip_until /12/           # -> 10
  #   s                           #
  #
  def skip_until(pattern)
    x = @pos || 0
    y = _scan(pattern, true, false)
    y.nil? ? nil : y - x
  end

  # Returns the string being scanned.
  def string
    @string
  end

  # Changes the string being scanned to +str+ and resets the
  # scanner. Returns +str+.
  def string=(str)
    unless str._isString
      str = Maglev::Type.coerce_to(str, String, :to_str)
    end
    reset
    @string = str
  end

  # Set the scan pointer to the end of the string and clear matching data.
  def terminate
    @match = nil
    @pos = @string.size
    self
  end

  # Set the scan pointer to the previous position. Only one previous
  # position is remembered, and it changes with each scanning operation.
  #
  #   s = StringScanner.new('test string')
  #   s.scan(/\w+/)        # => "test"
  #   s.unscan
  #   s.scan(/../)         # => "te"
  #   s.scan(/\d/)         # => nil
  #   s.unscan             # ScanError: unscan failed: previous match had failed
  def unscan
    raise ScanError if @match.equal?(nil)
    @pos = @prev_pos
    @prev_pos = nil
    @match = nil
    self
  end

  # Extracts a string corresponding to <tt>string[pos,len]</tt>, without
  # advancing the scan pointer.
  #
  #   s = StringScanner.new('test string')
  #   s.peek(7)          # => "test st"
  #   s.peek(7)          # => "test st"
  #
  def peek(len)
    unless len._isFixnum
      raise RangeError,'expected Fixnum'
    end
    raise ArgumentError if len < 0
    return "" if len.equal?(0)
    return @string[@pos, len]
  end

  # Equivalent to #peek. This method is obsolete; use #peek instead.
  def peep len
    warn "StringScanner#peep is obsolete; use #peek instead" if $VERBOSE
    peek len
  end

  def _scan(pattern, succptr, getstr)
    if pattern._isString
      # ok
    elsif pattern._isRegexp
      # ok
    else
      pattern = Maglev::Type.coerce_to(pattern, String, :to_str)
    end
    @match = nil

    ppos = @pos
    sstr = @string
    return nil if (sstr.size - ppos) < 0 # TODO: make more elegant

    # rest = self.rest
    # @match = pattern.match(rest)

    mmatch = pattern.match_from_nocheck(sstr, ppos)
    @match = mmatch

    return nil unless  mmatch

    m = rest[0, mmatch.end(0)]

    if succptr then
      @prev_pos = ppos
      @pos = mmatch.end(0)
    end

    if getstr then
      m
    else
      m.size
    end
  end
  private :_scan

  def _scan_headonly(pattern, succptr, getstr)
    unless pattern._isRegexp
      raise TypeError, "expected a Regexp"
    end
    @match = nil


    ppos = @pos
    sstr = @string

    return nil if (sstr.size - ppos) < 0 # TODO: make more elegant

    # rest = self.rest
    rest = sstr[ppos..-1]

    # NOTE - match_start is an Oniguruma feature that Rubinius exposes.
    # We use it here to avoid creating a new Regexp with '^' prepended.
    mmatch = pattern.match_start(rest, 0)
    @match = mmatch

    return nil unless  mmatch

    m = rest[0, mmatch.end(0)]

    if succptr then
      @prev_pos = ppos
      @pos += m.size
    end

    if getstr then
      m
    else
      m.size
    end
  end

  private :_scan_headonly
end
