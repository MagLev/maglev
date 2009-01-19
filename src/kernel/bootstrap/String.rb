class String

  # misc
  primitive 'hash'

  def to_rx
    Regexp.new(self)
  end

  primitive 'size=', 'size:'

  primitive_nobridge 'substring1', 'copyFrom:to:'
  primitive_nobridge '_findStringStartingAt', 'findString:startingAt:'
  class_primitive_nobridge '_withAll', 'withAll:'
  class_primitive_nobridge '_alloc', '_basicNew'

  def self.new(str)
    _withAll str
  end

  def self.new()
    # no bridge methods for this variant
    _alloc
  end

  def signal
    raise RuntimeError, self
  end

  # Class Methods

  # Instance Methods
  def %(arg)
    unless arg._isArray
      arg = [ arg ]
    end
    sprintf(self, *arg)
  end

  def *(n)
    n = Type.coerce_to(n, Integer, :to_int)
    unless n._isFixnum
      if n._isInteger
        raise RangeError , 'arg exceeds max Fixnum'
      end
    end
    if (n < 0)
      raise ArgumentError , 'arg must be positive'
    end
    str = self.class.new
    k = 0
    while k < n
      str << self
      k = k + 1
    end
    str
  end

  primitive_nobridge '_append', ','

  def +(o)
    other = Type.coerce_to(o, String, :to_str)
    _append other
  end

  # note smalltalk addAll:  returns arg, not receiver
  primitive_nobridge '<<', '_rubyAddAll:'

  # TODO: Need primitive ST implemention for <=>

  def <=>(o)
    if o._isString
      i = 0
      lim = size > o.size ? o.size : size # lim is the min
      while i < lim
        result = self[i] <=> o[i]
        return result unless result.equal?(0)
        i += 1
      end
      return size <=> o.size
    else
      if o.equal?(nil)
        return nil
      end
      # From Rubinius...there are a lot of strange things in how
      # string handles <=>...
      return nil unless o.respond_to?(:to_str) && o.respond_to?(:<=>)
      return nil unless tmp = (o <=> self)
      return -tmp
    end
  end

  # PERFORMANCE: String#== could use help
  primitive '==', '='  # TODO: this is incorrect...
  #   def ==(other)
  #     if (other._isString )
  #       (self <=> other) == 0
  #     elsif other.respond_to? :to_str
  #       other == str
  #     else
  #       false
  #     end
  #   end

#  alias === ==

  # =~ is  translated to  :match  Sexpression by parser ...

  primitive_nobridge '[]' , '_rubyAt:'
  primitive_nobridge '[]' , '_rubyAt:length:'

  primitive_nobridge '[]=', '_rubyAt:put:'
  primitive_nobridge '[]=', '_rubyAt:length:put:'

  # MNI: String#~

  primitive 'capitalize', 'rubyCapitalize'

  def capitalize!
    x = capitalize
    return nil if x == self
    replace(x)
  end

  primitive 'casecmp', 'equalsNoCase:'

  primitive '_atEquals', 'at:equals:'

  def chomp(rs=$/)
    # check for nil and '' before doing rs[0] in elsif
    if rs.equal?(nil) || rs.empty?
      return self.dup
    elsif rs[0].equal?(0xa)
      if rs.length.equal?(1)
        # the default record separator
        if self[-1].equal?(0xa)
          if self[-2].equal?(0xd)
            return self[0, self.length - 2 ]
          else
            return self[0, self.length - 1 ]
          end
        end
        return self.dup
      end
    end
    len = self.length
    rsLen = rs.length
    if len >= rs.length
      idx = self.length - rs.length # one based
      if self._atEquals(idx, rs)
        return self[0, idx]
      end
    end
    return self.dup
  end

  def chomp!(rs=$/)
    # check for nil and '' before doing rs[0] in elsif
    if rs.equal?(nil) || rs.empty?
      return self.dup
    elsif rs[0].equal?(0xa)
      if rs.length.equal?(1)
        # the default record separator
        lastCh = self[-1]
        if lastCh.equal?(0xa)
          if self[-2].equal?(0xd)
            self.size=(self.length - 2 )
          else
            self.size=(self.length - 1 )
          end
          return self
        end
      end
    else
      len = self.length
      rsLen = rs.length
      if len >= rs.length
        idx = self.length - rs.length # one based
        if self._atEquals(idx, rs)
          self.size=(idx)
          return self
        end
      end
    end
    return nil # no modification made
  end


  def chop
    mySize = self.length
    if mySize > 0
      if self[-1].equal?(0xa)
        if mySize > 1 && self[-2].equal?(0xd)
      return self[0, mySize - 3]
    else
      return self[0, mySize - 2]
        end
      else
        return self[0, mySize - 1]
      end
    else
      return self.dup
    end
  end

  def chop!
    mySize = self.length
    if mySize > 0
      if self[-1].equal?(0xa)
        if mySize > 1 && self[-2].equal?(0xd)
      self.size=(mySize - 2)
    else
      self.size=(mySize - 1)
        end
      else
        self.size=(mySize - 1)
      end
      return self
    end
    return nil # no modification made
  end

  primitive 'concat', '_rubyAddAll:'

  # arg to rubyCount: is expected to be an Array , so declare as 'count*'
  primitive 'count*', 'rubyCount:'

  # MNI: crypt

  primitive 'delete*',  'rubyDelete:'
  primitive 'delete!*', 'rubyDeleteInPlace:'

  # asLowercase is a smalltalk to:do: loop in CharacterCollection
  primitive 'downcase', 'asLowercase'
  primitive 'downcase!', 'rubyDowncaseInPlace'

  primitive '_dumpInto' , 'rubyDumpInto:'

  def dump
    res = self.class.new
    self._dumpInto(res)
    res
  end

  def each(sep=$/, &block)
    tokens = sep._split_string(self, nil)
    n = 0
    lim = tokens.size
    while n < lim
      block.call( tokens[n] )
      n = n + 1
    end
  end

  def each_byte
    n = 0
    lim = self.size
    while n < lim
      yield self[n]
      n = n + 1
    end
  end

  # each_char appears to be a Rubinius extension
  #  for each character of self, pass a one character String
  #  containing that character to the block
  def each_char(&blk)
    n = 0
    lim = self.size
    while n < lim
      temp = ' '
      temp[0] = self[n]
      blk.call(temp)
      n = n + 1
    end
  end

  def each_line(&b)
    # TODO why different than each  ???
    /(.*)/.all_matches(self).each do |match|
      str = match[1]
      b.call(str)
    end
  end

  primitive 'empty?', 'isEmpty'
  primitive 'eql?', '='

  def gsub(regex, str)
    out = ""
    start = 1
    get_pattern(regex, true).each_match(self) do |match|
      out << substring1(start, match.begin(0))
      out << str
      start = match.end(0) + 1
    end
    if start <= length
      out << substring1(start, length)
    end
    out
  end

  def gsub(regex, &block)
    out = ""
    start = 1
    get_pattern(regex, true).each_match(self) do |match|
      out << substring1(start, match.begin(0))
      saveTilde = block._fetchRubyVcGlobal(0);
      begin
        block._setRubyVcGlobal(0, match);
        out << block.call.to_s
      ensure
        block._setRubyVcGlobal(0, saveTilde);
      end
      start = match.end(0) + 1
    end
    if start <= length
      out << substring1(start, length)
    end
    out
  end

  # From Rubinius
  def get_pattern(pattern, quote = false)
    unless pattern._isString || pattern._isRegexp
      if pattern.respond_to?(:to_str)
        pattern = pattern.to_str
      else
        raise TypeError, "wrong argument type #{pattern.class} (expected Regexp)"
      end
    end
    pattern = Regexp.quote(pattern) if quote && pattern._isString
    pattern = Regexp.new(pattern) unless pattern._isRegexp
    pattern
  end

  def gsub!(regex, str)
    replace(gsub(regex, str))
  end

  def gsub!(regex, &block)
    replace(gsub(regex, &block))
  end

  def hex
    # Because 0b1 is a proper hex number, rather than the binary number 1,
    # we repeat code here and tweak for hex.  Only 0X and 0x should be removed.
    s = self.delete('_').strip
    s =~ /^([+-]?)(0[xX])?([[:xdigit:]]*)/
    "16r#{$1}#{$3}"._to_i
  end

  def include?(item)
    !self.index(item).equal?(nil)
  end

  primitive_nobridge '_indexOfByte', 'indexOfByte:startingAt:'

  def index(item, offset=0)
    offset = Type.coerce_to(offset, Integer, :to_int)
    if offset < 0
      offset = self.size + offset
    end
    if item._isString
      st_idx = self._findStringStartingAt(item, offset + 1)
    elsif item._isInteger
      st_idx = self._indexOfByte(item % 256 , offset + 1)
    else
      # item should be a Regex
      return item._index_string(string, offset)
    end
    if st_idx.equal?(0)
      return nil
    else
      return st_idx - 1
    end
  end

  primitive '_insertAllAt', 'insertAll:at:'
  def insert(index, string)
    # account for smalltalk index
    idx = index < 0 ? index + size + 2 : index + 1
    raise IndexError, "index #{index} out of string" if idx <= 0 || idx > size + 1
    _insertAllAt(string, idx) # Flip order of parameters
    self
  end

  primitive 'intern', 'asSymbol'
  primitive 'length', 'size'

  primitive 'lstrip', 'trimLeadingSeparators'
  primitive 'lstrip!', '_removeLeadingSeparators' # in .mcz

  def match(pattern)
    # TODO: Figure out how to pass something more complex than a method name
    # to coerce_to.  A proc? A proc or a symbol?
    #p = Type.coerce_to(pattern, Regexp, "Regexp.new(:to_str)" )
    if pattern._isRegexp    # if pattern.kind_of?(Regexp)
      regexp = pattern
    else
      # TODO more optimization of coerce here
      begin
        regexp = Regexp.new(pattern.to_str)
        rescue StandardError
          raise TypeError, "wrong argument type #{pattern.class} (expected Regexp)"
      end
    end
    regexp.match self
  end

  # MNI: next
  # MNI: next!

  def oct
    base, s = self.extract_base(8)
    "#{base}r#{s}"._to_i
  end

  primitive 'replace', '_rubyReplace:'
  primitive 'reverse', 'reverse'

  def reverse!
    replace(reverse)
  end

  primitive_nobridge '_lastSubstring', 'findLastSubString:startingAt:'
  primitive_nobridge '_indexOfLastByte', 'indexOfLastByte:startingAt:'

  def rindex(item, offset=nil)
    if offset.equal?(nil) 
      offset = self.size 
      if (offset.equal?(0))
        offset = 1
      end
    else
      offset = Type.coerce_to(offset, Integer, :to_int)
      if offset < 0
        offset = self.size + offset
      end
      offset = offset + 1  # to one-based
    end
    if item._isString
      if item.size.equal?(0)
        if self.size.equal?(0)
          return 0  
        else
          return offset
        end
      end
      st_idx = self._lastSubstring(item, offset )
    elsif item._isInteger
      st_idx = self._indexOfLastByte(item % 256 , offset )
    else
      raise NotImplementedError , 'String#rindex(aRegexp) not implemented' 
    end 
    if st_idx.equal?(0)
      return nil
    else
      return st_idx - 1
    end
  end

  primitive 'rstrip', 'trimTrailingSeparators'
  primitive 'rstrip!', '_removeTrailingSeparators'  # in .mcz

#   def scan(regex)
#     result = []
#     regex.to_rx.each_match(self) do |m|
#       result << m[0]
#     end
#     result
#   end

#   def scan(regex, &blk)
#     regex.to_rx.each_match(self) do |m|
#       yield m[0]  # m is a matchdata, m[0] is the matched string
#     end
#     self
#   end

  primitive 'size', 'size'

  primitive_nobridge 'slice', '_rubyAt:'
  primitive          'slice', '_rubyAt:length:'

  def slice!(*args)
    replace(slice(*args).to_str)
  end

  def split(pattern=nil, limit=nil)
    # BEGIN RUBINIUS
    return [] if size.equal?(0)

    if limit
      if !limit.kind_of?(Integer) and limit.respond_to?(:to_int)
        limit = limit.to_int
      end

      if limit > 0
        return [self.dup] if limit == 1
        limited = true
      else
        limited = false
      end
    else
      limited = false
    end

    pattern ||= ($; || " ")

    if pattern == ' '
      spaces = true
      pattern = /\s+/
    elsif pattern.nil?
      pattern = /\s+/
    elsif pattern.kind_of?(Regexp)
      # Pass
    else
      pattern = StringValue(pattern) unless pattern.kind_of?(String)
      pattern = Regexp.new(Regexp.quote(pattern))
    end

    start = 0
    ret = []

    last_match = nil

    while match = pattern.match_from(self, start)
      break if limited && limit - ret.size <= 1

      collapsed = match.collapsing?

      if !collapsed || (match.begin(0) != 0)
        ret << match.pre_match_from(last_match ? last_match.end(0) : 0)
        ret.push(*match.captures.compact)
      end

      if collapsed
        start += 1
      elsif last_match && last_match.collapsing?
        start = match.end(0) + 1
      else
        start = match.end(0)
      end

      last_match = match
    end

    if !last_match.nil?           # GEMSTONE
      pm = last_match.post_match  # GEMSTONE
      ret << (pm.nil? ? "" : pm)  # GEMSTONE
    elsif ret.empty?
      ret << self.dup
    end

    # Trim from end
    if !ret.empty? and (limit == 0 || limit.nil?)
      while s = ret.last and s.empty?
        ret.pop
      end
    end

    # Trim from front
    if !ret.empty? and spaces
      while s = ret.first and s.empty?
        ret.shift
      end
    end

    # BEGIN GEMSTONE
    # If we are matching the empty string, and we have matches, then
    # we need to tack on the trailing empty string match.
    if ret && limit && limit < 0 && last_match && last_match.collapsing?
      ret << ''
    end
    # END GEMSTONE

    # Support subclasses
    ret = ret.map { |str| self.class.new(str) } if !self.instance_of?(String)

    # Taint all
    ret = ret.map { |str| str.taint } if self.tainted?

    ret
  end

  def _split_string(string, limit)
    Regexp.new(self)._split_string(string, limit)
  end

  primitive_nobridge 'squeeze', 'rubySqueeze'
  primitive 'squeeze*', 'rubySqueeze:'

  primitive_nobridge 'squeeze!', 'rubySqueezeSelf'
  primitive 'squeeze!*', 'rubySqueezeSelf:'

  def strip
    _strip
  end
  primitive '_strip', 'withBlanksTrimmed'

  def strip!
    replace(strip)
  end

  def sub(regex, str, &block)
    if match = regex.to_rx.match(self)
      out = ""
      out << self[0...(match.begin(0))]
      if block
        out << block.call.to_s
      else
        out << str
      end
      out << ((self[(match.end(0))...length]) || "")
      out
    else
      dup
    end
  end

  def sub!(regex, str, &block)
    new = sub(regex, str, &block)
    if new == self
      nil
    else
      replace(new)
      self
    end
  end


  primitive 'succ!', 'rubySucc'

  def succ
    d = self.dup
    d.succ!
  end

  def sum(power=16)
    tot = 0
    n = 0
    lim = self.size
    unless power._isFixnum
      power = power.to_int
    end
    mod = (1 << power) - 1
    while n < lim
      tot = tot + self[n]
      tot = tot & mod
      n = n + 1
    end
    tot
  end

  # MNI: swapcase
  # MNI: swapcase!

  primitive 'to_f', 'asFloat'
  def to_i(base=10)
    base = Type.coerce_to(base, Integer, :to_int)
    raise ArgumentError, "illegal radix #{base}" if base < 0 || base == 1 || base > 36
    self.to_inum(base, false)
  end

  primitive_nobridge '_to_i', 'asInteger'

  # Consider self as an integer and return value given base.
  # This is the rubinius API, but we don't care about the check param
  def to_inum(base, check=false)
    if base == 0
      base, s = self.extract_base
    else
      s = self
    end
    "#{base}r#{s}"._to_i
  end

  # Return an array of two elements: [an_int, a_string], where an_int is
  # base, unless self contains a base specifier (e.g., "0x", "0b", etc.),
  # in which case, an_int is the appropriate base.  a_string is self with
  # any base specifier removed.
  #
  # "0x10" => [16, 10]
  # "-0b1010".extract_base     => [2, "-1010"]
  # "-0b1010".extract_base(16) => [2, "-1010"]
  # "-1010".extract_base(16)   => [16, "-1010"]
  def extract_base(base=10)
    s = self.delete('_').strip
    s =~ /^([+-]?)(0[bdox])?(.*)/i
    base = {"0b" => 2, "0d" => 10, "0o" => 8, "0x" => 16}[$2.downcase] unless $2.nil?
    [base, "#{$1}#{$3}"]
  end

  def to_s
    self
  end

  def to_str
    self
  end

  primitive_nobridge 'to_sym', 'asSymbol'

  primitive 'tr!', 'rubyTrFrom:to:'

  def tr(from, to)
    dup.tr!(from, to)
  end

  primitive 'tr_s!', 'rubyTrSqueezeFrom:to:'
  def tr_s(from, to)
    (str = self.dup).tr_s!(from, to) || str
  end

  primitive 'unpack', 'rubyUnpack:'
  primitive 'upcase', 'asUppercase'
  primitive 'upcase!', 'rubyUpcaseInPlace'

  # MNI: upto

  # ====== Object
  primitive '_inspect', '_rubyPrintString'

  def inspect(touchedSet=nil)
    _inspect
  end

  primitive 'dup', 'copy'

  # ====== Comparable:
  # RxINC: This is a cut-n-paste to get things working for mspec.
  # Need to either overwrite or allow a mixin.

  def >(other)
    (self <=> other) > 0
  end

  def <(other)
    (self <=> other) < 0
  end

  def >=(other)
    (self <=> other) >= 0
  end

  def <=(other)
    (self <=> other) <= 0
  end

  def between?(min, max)
    (min <= self) && (self <= max)
  end

  ###### Rubinius Code Here

  def rjust(width, padstr = " ")
    justified = dup
    justified.justify(width, :right, padstr)
  end

  def ljust(width, padstr = " ")
    justified = dup
    justified.justify(width, :left, padstr)
  end

  def center(width, padstr = " ")
    centered = dup
    centered.justify(width, :center, padstr)
  end

  primitive "_paddedToWithString", "padded:to:withString:"

  # This started off as Rubinius, but was heavily modified since most
  # work is done in smalltalk.
  def justify(width, direction, padstr=" ")
    padstr = StringValue(padstr)
    raise ArgumentError, "zero width padding" if padstr.size.equal?(0)

    width = Type.coerce_to(width, Integer, :to_int) unless width._isFixnum
    sz = size
    if width > sz
      padsize = width - sz
    else
      return dup
    end

    _paddedToWithString(direction, width, padstr)
    taint if padstr.tainted?
    self
  end
end
