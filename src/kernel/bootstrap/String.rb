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
  def %(args)
    a = Array(args).dup
    gsub(/%(\d|\.)*./){a.shift.to_fmt}
  end

  def *(n)
    str = ""
    n.times{str << self}
    str
  end

  primitive '+', ','

  # note smalltalk addAll:  returns arg, not receiver
  primitive '<<', '_rubyAddAll:'

  # TODO: Need to find ST implemention for <=>
  def <=>(o)
    other = Type.coerce_to(o, String, :to_s)

    i = 0
    lim = size > other.size ? other.size : size # lim is the min
    while i < lim
      result = self[i] <=> other[i]
      return result if result != 0
      i += 1
    end
    size <=> other.size
  end

  # PERFORMANCE: String#== could use help
  primitive '==', '='  # TODO: this is incorrect...
  #   def ==(other)
  #     if other.kind_of?(String)
  #       (self <=> other) == 0
  #     elsif other.respond_to? :to_str
  #       other == str
  #     else
  #       false
  #     end
  #   end

#  alias === ==

  # =~ is implemented somewhere....

  primitive '[]' , '_rubyAt:'
  primitive '[]' , '_rubyAt:length:'

  primitive '[]=', '_rubyAt:put:'
  primitive '[]=', '_rubyAt:length:put:'

  # MNI: String#~

  primitive 'capitalize', 'rubyCapitalize'

  def capitalize!
    replace(capitalize)
  end

  primitive 'casecmp', 'equalsNoCase:'

  def chomp
    if self[-1] == ?\r
      return self[0..-2]
    end

    if self[-1] == ?\n
      if self[-2] == ?\r
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

  def gsub(regex, str, &block)
    out = ""
    start = 1
    regex.to_rx.each_match(self) do |match|
      out << substring1(start, match.begin(0))
      if block
        out << block.call.to_s
      else
        out << str
      end
      start = match.end(0) + 1
    end
    if start <= length
      out << substring1(start, length)
    end
    out
  end

  def gsub!(regex, str, &block)
    replace(gsub(regex, str, &block))
  end

  def hex
    _to_i(16)
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
    puts "==== index #{index}  idx #{idx}"
    raise IndexError, "index #{index} out of string" if idx <= 0 || idx > size + 1
    _insertAllAt(string, idx) # Flip order of parameters
    self
  end

  primitive 'intern', 'asSymbol'
  primitive 'length', 'size'

  primitive 'lstrip', 'trimLeadingSeparators'

  # TODO: PERFROMANCE: In the case that there is leading whitespace, the
  # underlying smalltalk returns a copy.  Is there a way to remove leading
  # w/o a copy?
  def lstrip!
    original_length = length
    replace(lstrip)
    return original_length == length ? nil : self
  end

  def match(pattern)
    # TODO: Figure out how to pass something more complex than a method name
    # to coerce_to.  A proc? A proc or a symbol?
    #p = Type.coerce_to(pattern, Regexp, "Regexp.new(:to_str)" )
    knd = pattern._kindBlkStrRanRegAry
    if (knd.equal?(2) )  # if pattern.kind_of?(Regexp)
      regexp = pattern
    elsif pattern.respond_to?(:to_str)
      # TODO more optimization of coerce here
      regexp = Regexp.new(pattern.to_str)
    else
      raise TypeError, "wrong argument type #{pattern.class} (expected Regexp)"
    end
    regexp.match self
  end

  # MNI: next
  # MNI: next!
  # MNI: oct   def oct; _to_i(8); end

  primitive 'replace', '_rubyReplace:'
  primitive 'reverse', 'reverse'

  def reverse!
    replace(reverse)
  end

  # MNI: rindex  TODO: use findLast: ?, but does a block eval
  def rindex(item, offset=nil)
    if offset.nil?
      item._lastIndexOf(item)
    else
      # TODO:!!!!
    end
  end

  primitive 'rstrip', 'trimTrailingSeparators'

  # TODO: PERFROMANCE: In the case that there is trailing whitespace, the
  # underlying smalltalk returns a copy.  Is there a way to remove leading
  # w/o a copy?
  def rstrip!
    original_length = length
    replace(rstrip)
    return original_length == length ? nil : self
  end

  def scan(regex)
    result = []
    regex.to_rx.each_match(self) do |m|
      result << m[0]
    end
    result
  end

  primitive 'size', 'size'

  primitive 'slice', '_rubyAt:'
  primitive 'slice', '_rubyAt:length:'

  def slice!(*args)
    replace(slice(*args).to_str)
  end

  def split(pattern=nil, limit=nil)
    return [] if empty?
    pattern ||= ($; || " ")

    pattern._split_string(self, limit)
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
  def to_i
    _to_i
  rescue Exception
    0
  end

  def to_i
    _to_i
  rescue Exception
    0
  end
  primitive '_to_i', 'asInteger'
  #  non-base-10 radix Ruby syntax not supported yet
  primitive '_to_i', '_asInteger:'

  def to_s
    self
  end

  def to_str
    self
  end

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
  primitive 'inspect', 'printString'
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
    raise ArgumentError, "zero width padding" if padstr.size == 0

    width = Type.coerce_to(width, Integer, :to_int) unless width.kind_of? Fixnum
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
