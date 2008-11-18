class String

  # misc
  primitive 'hash'

  def to_rx
    Regexp.new(self)
  end

  primitive 'size=', 'size:'

  primitive 'substring1', 'copyFrom:to:'
  primitive '_findStringStartingAt', 'findString:startingAt:'

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
    str = ""
    k = 0
    while k < n
      str << self
      k = k + 1
    end
    str
  end

  primitive '+', ','

  # note smalltalk addAll:  returns arg, not receiver
  primitive_nobridge '<<', '_rubyAddAll:'

  # TODO: Need primitive ST implemention for <=>
  def <=>(o)
    other = Type.coerce_to(o, String, :to_s)

    i = 0
    lim = size > other.size ? other.size : size # lim is the min
    while i < lim
      result = self[i] <=> other[i]
      return result unless result.equal?(0)
      i += 1
    end
    size <=> other.size
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
    replace(capitalize)
  end

  primitive 'casecmp', 'equalsNoCase:'

  def chomp
    if self[-1].equal?("\r")
      return self[0..-2]
    end

    if self[-1].equal?("\n")
      if self[-2].equal?("\r")
        return self[0..-3]
      else
        return self[0..-2]
      end
    end

    return self
  end

  def chomp!
    replace(chomp)
  end

  def chop
    self[0..size-2]
  end

  def chop!
    replace(chop)
  end

  primitive 'concat', '_rubyAddAll:'

  # arg to _rubyCount: is expected to be an Array , so declare as 'count*'
  primitive 'count*', 'rubyCount:'

  # MNI: crypt

  primitive 'delete*',  'rubyDelete:'
  primitive 'delete!*', 'rubyDeleteInPlace:'

  # asLowercase is a smalltalk to:do: loop in CharacterCollection
  primitive 'downcase', 'asLowercase'
  primitive 'downcase!', 'rubyDowncaseInPlace'

  # MNI: dump

  def each(sep=$/, &block)
    tokens = sep._split_string(self, nil)
    tokens.each { |t| block.call(t) }
  end

  def each_byte
    0.upto(size-1) do |idx|
      yield self[idx]
    end
  end

  # TODO: what is each_char?
  def each_char
    each_byte do |b|
      temp = ' '
      temp[0] = b
      yield temp
    end
  end

  def each_line(&b)
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
    regex.to_rx.each_match(self) do |match|
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
    regex.to_rx.each_match(self) do |match|
      out << substring1(start, match.begin(0))
      out << block.call.to_s
      start = match.end(0) + 1
    end
    if start <= length
      out << substring1(start, length)
    end
    out
  end

  def gsub!(regex, str)
    replace(gsub(regex, str))
  end

  def gsub!(regex, &block)
    replace(gsub(regex, &block))
  end

  def hex
    self.sub(/0x/i, '').to_inum(16, false)
  end

  def include?(item)
    !self.index(item).equal?(nil)
  end

  def index(item, offset=nil)
    if offset.equal?(nil)
      item._index_string(self, 0)
    else
      offset = size + offset if offset < 0
      string = self[offset..-1]
      return item._index_string(string, offset)
    end
  end

  def _index_string(string)
    string._findStringStartingAt(self, 1) - 1
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
    if ( pattern._isRegexp  )  # if pattern.kind_of?(Regexp)
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
    self.to_inum(8, false)
  end

  primitive 'replace', '_rubyReplace:'
  primitive 'reverse', 'reverse'

  def reverse!
    replace(reverse)
  end

  # MNI: rindex  TODO: use findLast: ?, but does a block eval
  primitive '_lastSubstring', 'findLastOccuranceOfString:startingAt:'
  def rindex(item, offset=0)
    return size if item.empty? # This must be before we check for self.empty?
    return nil if self.empty?
    # TODO: Need to coerce to string....
    # arg = StringValue(arg) unless [Fixnum, String, Regexp].include?(arg.class)
    result = _lastSubstring(item, offset + 1)
    return  result == 0 ? nil : result - 1

    # TODO: support for when item is a regexp
    # TODO: support for when item is an int ("character")
  end

  primitive 'rstrip', 'trimTrailingSeparators'
  primitive 'rstrip!', '_removeTrailingSeparators'  # in .mcz

  def scan(regex)
    result = []
    regex.to_rx.each_match(self) do |m|
      result << m[0]
    end
    result
  end

  def scan(regex, &blk)
    regex.to_rx.each_match(self) do |m|
      yield m[0]  # m is a matchdata, m[0] is the matched string
    end
    self
  end

  primitive 'size', 'size'

  primitive_nobridge 'slice', '_rubyAt:'
  primitive          'slice', '_rubyAt:length:'

  def slice!(*args)
    replace(slice(*args).to_str)
  end

  def split(pattern=nil, limit=nil)
    # BEGIN RUBINIUS
    return [] if empty?

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

  # MNI: squeeze
  # MNI: squeeze!

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


  # MNI: succ
  # MNI: succ!
  # MNI: sum
  # MNI: swapcase
  # MNI: swapcase!

  primitive 'to_f', 'asFloat'
  def to_i(base=10)
    base = Type.coerce_to(base, Integer, :to_int)
    raise ArgumentError, "illegal radix #{base}" if base < 0 || base == 1 || base > 36
    self.to_inum(base, false)
  end

  primitive_nobridge '_to_i', 'asInteger'
  #  non-base-10 radix Ruby syntax not supported yet
  primitive '_to_i', '_asInteger:'

  # Consider self as an integer and return value given base.
  # This is the rubinius API, and some code taken from rubinius.
  def to_inum(base, check)
    detect_base = true if base == 0

    # Ruby lets you add '_' to numbers to look nice, we strip them
    s = if check then
          self.strip
        else
          self.delete('_').strip
        end

    if detect_base then
      # $1 is sign, $2 is base specifier $3 is the rest
      s =~ /^([+-]?)(0[bdox]?)?(.*)/i
      base = {"0b" => 2, "0d" => 10, "0o" => 8, '0' => 8, "0x" => 16}[$2.downcase]
      s = "#{$1}#{$3}"  # The sign and number w/o base specifier
    end
    raise ArgumentError, "illegal radix #{base}" unless (2..36).include? base
    "#{base}r#{s}"._to_i  # convert to smalltalk form and convert
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
  primitive 'inspect', '_rubyPrintString'
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
