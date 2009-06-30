class String

  # misc
  primitive 'hash'

  def to_rx
    Regexp.new(self)
  end

  primitive 'size=', 'size:'

  primitive_nobridge 'substring1', 'copyFrom:to:'
  primitive_nobridge '_findStringStartingAt', 'findString:startingAt:'
  primitive_nobridge '_md5sum', 'md5sum'
  primitive_nobridge '_remove_from_to', 'removeFrom:to:'
  class_primitive_nobridge '_withAll', 'withAll:'
  class_primitive_nobridge '_alloc', '_basicNew'

  def self.new(str)
    if self.equal?(String)
      s = _withAll(str)
    else
      s = _alloc
    end
    s.initialize(str)
    s
  end

  def initialize(str)
    if self.class.equal?(String)
      # do nothing
    else
      self.replace(str)
    end
    self
  end

  def self.new()
    # no bridge methods for this variant
    s = _alloc
    s.initialize
    s
  end

  def initialize
    self
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

  primitive_nobridge '_concatenate', ','

  def +(o)
    other = Type.coerce_to(o, String, :to_str)
    self._concatenate(other)
  end

  # note smalltalk addAll:  returns arg, not receiver
  #primitive '_append', '_rubyAddAll:'
  primitive '_append', '_rubyAddAll:'

  def <<(arg)
    raise TypeError, "<<: can't modify frozen string" if self.frozen?
    if arg._isFixnum
      raise TypeError, "<<: #{arg} out of range" if arg < 0 or arg > 255
      other = arg
    else
      other = Type.coerce_to(arg, String, :to_str)
    end
    self._append(other)
    # self.taint if other.tainted?
    self
  end

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
      return nil if o._isSymbol
      return nil unless o.respond_to?(:to_str) && o.respond_to?(:<=>)
      return nil unless tmp = (o <=> self)
      return -tmp
    end
  end

  # PERFORMANCE: String#== could use help
  primitive '_same_value', '='  # TODO: this is incorrect...
  def ==(other)
    if other._isString
      s = other
    elsif other.respond_to? :to_str
      s = Type.coerce_to(other, String, :to_str)
    else
      return false
    end
    self._same_value(s)
  end

#  alias === ==

  #    str =~ obj   => fixnum or nil
  #
  # Match---If <i>obj</i> is a <code>Regexp</code>, use it as a pattern to match
  # against <i>str</i>,and returns the position the match starts, or
  # <code>nil</code> if there is no match. Otherwise, invokes
  # <i>obj.=~</i>, passing <i>str</i> as an argument. The default
  # <code>=~</code> in <code>Object</code> returns <code>false</code>.
  #
  #    "cat o' 9 tails" =~ /\d/   #=> 7
  #    "cat o' 9 tails" =~ 9      #=> false
  def =~(*args, &blk)
    # only one-arg call supported. any other invocation
    # will have a bridge method interposed which would
    #   require different args to _storeRubyVcGlobal
    raise ArgumentError, 'expected 1 arg'
  end

  def =~(other)
    # no bridge method for this variant
    # =~ is mostly translated to  :match  Sexpression by parser ...
    if other._isRegexp
      m = other._search(self, 0, nil)
      m._storeRubyVcGlobal(0x20) # store into caller's $~
      if (m)
        return m.begin(0)
      end
      m
    elsif other._isString
      raise TypeError, 'String given'
    else
      super(other) # code args explicitly to avoid implicit block of ZSuperNode
    end
  end

  primitive_nobridge '_at' , '_rubyAt:'
  primitive_nobridge '_at_length' , '_rubyAt:length:'

  def [](*args)
    # This variant gets bridge methods
    raise ArgumentError, 'wrong number of arguments'
  end

  def [](index)
    if index._isRegexp
      s = _at(index)
      # s.taint if index.tainted?
    elsif index._isRange
      first, len = index._beg_len(self.length)
      return nil if first.equal?(nil)
      s = self._at_length(first, len)
    elsif index._isString
      s = self._at(index)
      # s.taint if ! s.nil? && index.tainted?
    else
      index = Type.coerce_to(index, Integer, :to_int)
      s = self._at(index)
    end
    # s.taint if self.tainted? and not s.nil?
    s
  end

  def [](start, length)
    if start._isRegexp
      m_begin, m_len = self._match_regexp(start, length)
      return nil if m_begin.equal?(nil)
      s = self._at_length(m_begin, m_len)
      # s.taint if start.tainted?
      s
    else
      length = Type.coerce_to(length, Integer, :to_int)
      start = Type.coerce_to(start, Integer, :to_int)
      s = _at_length(start, length)
      return nil if length < 0
    end
    # s.taint if self.tainted?
    s
  end

  primitive_nobridge '_at_put', '_rubyAt:put:'
  primitive_nobridge '_at_length_put', '_rubyAt:length:put:'
  def []=(*args)
    # This variant gets bridge methods
    raise ArgumentError, 'wrong number of arguments'
  end

  def []=(index, value)
    val = value._isFixnum ? value : Type.coerce_to(value, String, :to_str)
    if index._isString or index._isRegexp
      _at_put(index, val)
    else
      idx = Type.coerce_to(index, Integer, :to_int)
      sz = self.size
      idx += sz if idx < 0
      raise IndexError, "index #{idx} out of string" if idx < 0 or idx > sz
      _at_put(idx, val)
    end
    # taint if value.tainted?
    self
  end

  def []=(index, count, value)
    if index._isRegexp
      _at_length_put(index, count, value)
    else
      idx = Type.coerce_to(index, Integer, :to_int)
      sz = self.size
      idx += sz if idx < 0

      raise IndexError, "index #{idx} out of string" if idx < 0 or idx > sz

      str_value = Type.coerce_to(value, String, :to_str)
      raise IndexError, "index #{count} out of string" if count < 0

      _at_length_put(idx, count, str_value)
    end
    # taint if value.tainted?
    self
  end

  # MNI: String#~

  primitive '_capitalize', 'rubyCapitalize'

  def capitalize
    x = _capitalize
    # x.taint if self.tainted?
    x
  end

  def capitalize!
    raise TypeError, "can't modify frozen string" if frozen?
    x = _capitalize
    return nil if x == self
    replace(x)
  end

  primitive 'casecmp', 'equalsNoCase:'

  primitive '_atEquals', 'at:equals:'

  # Returns a new +String+ with the given record separator removed from the
  # end of receiver (if present).  If <tt>$/</tt> has not been changed from
  # the default Ruby record separator, then +chomp+ also removes carriage
  # return characters (that is, it will remove \n, \r, and \r\n).
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
        return self[0, self.length - 1 ] if self[-1].equal?(0xd) # "...\r"
        return self.dup
      end
    end
    len = self.length
    rsLen = rs.length
    if len >= rs.length
      idx = self.length - rs.length # zero based
      if self._atEquals(idx+1, rs)
        return self[0, idx]
      end
    end
    return self.dup
  end

  def chomp!(rs=$/)
    raise TypeError, "can't modify frozen string" if frozen?
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
    raise TypeError, "can't modify frozen string" if frozen?
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

  alias concat <<

  # arg to rubyCount: is expected to be an Array , so declare as 'count*'
  primitive 'count*', 'rubyCount:'

  # MNI: crypt

  primitive 'delete*',  'rubyDelete:'
  primitive 'delete!*', 'rubyDeleteInPlace:'

  # asLowercase is a smalltalk to:do: loop in CharacterCollection
  primitive '_downcase', 'asLowercase'
  primitive '_downcase!', 'rubyDowncaseInPlace'

  def downcase
    s = _downcase
    # s.taint if self.tainted?
    s
  end

  def downcase!
    raise TypeError, "can't modify frozen string" if frozen?
    _downcase!
  end

  primitive '_dumpInto' , 'rubyDumpInto:'

  def dump
    res = self.class.new
    self._dumpInto(res)
    res
  end

  # Splits <i>self</i> using the supplied parameter as the record separator
  # (<code>$/</code> by default), passing each substring in turn to the supplied
  # block. If a zero-length record separator is supplied, the string is split on
  # <code>\n</code> characters, except that multiple successive newlines are
  # appended together.
  #
  #   print "Example one\n"
  #   "hello\nworld".each {|s| p s}
  #   print "Example two\n"
  #   "hello\nworld".each('l') {|s| p s}
  #   print "Example three\n"
  #   "hello\n\n\nworld".each('') {|s| p s}
  #
  # <em>produces:</em>
  #
  #   Example one
  #   "hello\n"
  #   "world"
  #   Example two
  #   "hel"
  #   "l"
  #   "o\nworl"
  #   "d"
  #   Example three
  #   "hello\n\n\n"
  #   "world"
  def each(a_sep=$/, &block)
    # Modified Rubinius
    if a_sep.equal?(nil)
      block.call(self)
      return self
    end

    sep = Type.coerce_to(a_sep, String, :to_str)
    raise LocalJumpError, 'no block given' unless block_given?

    id = self.__id__
    my_size = self.size
    ssize = sep.size
    newline = ssize == 0 ? ?\n : sep[ssize-1]

    last, i = 0, ssize
    while i < my_size
      if ssize == 0 && self[i] == ?\n
        if self[i+=1] != ?\n
          i += 1
          next
        end
        i += 1 while i < my_size && self[i] == ?\n
      end

      if i > 0 && self[i-1] == newline &&
          (ssize < 2 || sep._compare_substring(self, i-ssize, ssize) == 0)
        line = self[last, i-last]
        # line.taint if tainted?
        yield line
        # We don't have a way yet to check if the data was modified...
        #modified? id, my_size
        last = i
      end

      i += 1
    end

    unless last == my_size
      line = self[last, my_size-last+1]
      # line.taint if tainted?
      yield line
    end

    self
  end
  alias each_line each

  def _compare_substring(other, start, size)
    if start > self.size || start + self.size < 0
      raise IndexError, "index #{start} out of string"
    end
    self <=> other[start,size]
  end

  def each_byte
    n = 0
    # Do not cache size before looping.  Specs require
    # us to go to new end when string grows or shrinks
    # in the yield.
    while n < self.size
      yield self[n]
      n = n + 1
    end
    self
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

  primitive 'empty?', 'isEmpty'
  primitive 'eql?', '='


  # Returns a copy of <i>self</i> with <em>all</em> occurrences of <i>pattern</i>
  # replaced with either <i>replacement</i> or the value of the block. The
  # <i>pattern</i> will typically be a <code>Regexp</code>; if it is a
  # <code>String</code> then no regular expression metacharacters will be
  # interpreted (that is <code>/\d/</code> will match a digit, but
  # <code>'\d'</code> will match a backslash followed by a 'd').
  #
  # If a string is used as the replacement, special variables from the match
  # (such as <code>$&</code> and <code>$1</code>) cannot be substituted into it,
  # as substitution into the string occurs before the pattern match
  # starts. However, the sequences <code>\1</code>, <code>\2</code>, and so on
  # may be used to interpolate successive groups in the match.
  #
  # In the block form, the current match string is passed in as a parameter, and
  # variables such as <code>$1</code>, <code>$2</code>, <code>$`</code>,
  # <code>$&</code>, and <code>$'</code> will be set appropriately. The value
  # returned by the block will be substituted for the match on each call.
  #
  # The result inherits any tainting in the original string or any supplied
  # replacement string.
  #
  #   "hello".gsub(/[aeiou]/, '*')              #=> "h*ll*"
  #   "hello".gsub(/([aeiou])/, '<\1>')         #=> "h<e>ll<o>"
  #   "hello".gsub(/./) {|s| s[0].to_s + ' '}   #=> "104 101 108 108 111 "
  def gsub(regex, str)
    out = ""
    start = 1
    get_pattern(regex, true).__each_match(self) do |match|
      out << substring1(start, match.begin(0))
      out << str._to_sub_replacement(match)
      start = match.end(0) + 1
    end
    if start <= length
      out << substring1(start, length)
    end
    out
  end

  # From Rubinius
  def _to_sub_replacement(match)
    index = 0
    result = ""
    lim = size
    while index < lim
      current = index
      while current < lim && self[current] != ?\\
        current += 1
      end
      result << self[index, current - index]
      break if current == lim

      # found backslash escape, looking next
      if current == lim - 1
        result << ?\\ # backslash at end of string
        break
      end
      index = current + 1

      result << case (cap = self[index])
        when ?&
          match[0]
        when ?`
          match.pre_match
        when ?'
          match.post_match
        when ?+
          match.captures.compact[-1].to_s
        when ?0..?9
          match[cap - ?0].to_s
        when ?\\ # escaped backslash
          '\\'
        else     # unknown escape
          '\\' << cap
      end
      index += 1
    end
    return result
  end

  def _replace_match_with(match, replacement)
    out = ""
    out << self[0...(match.begin(0))]
    unless replacement.equal?(nil)
      out << replacement._to_sub_replacement(match)
    end
    out << ((self[(match.end(0))...length]) || "")
    out
  end


  def gsub(regex, &block)
    # $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    start = 1
    out = ''
    get_pattern(regex, true).__each_match_vcgl(self, 0x30) do |match|
      out << substring1(start, match.begin(0))
      saveTilde = block._fetchRubyVcGlobal(0);
      begin
        block._setRubyVcGlobal(0, match);
        out << block.call(match[0]).to_s
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
    raise TypeError, "can't modify frozen string" if frozen?
    replace(gsub(regex, str))
  end

  def gsub!(regex, &block)
    raise TypeError, "can't modify frozen string" if frozen?
    replace(gsub(regex, &block))
  end

  def _delete_underscore_strip
    str = self
    idx = str._indexOfByte( ?_ , 1 )
    unless idx.equal?(0)
      str = str.delete('_')
    end
    str.strip
  end

  def hex
    # Because 0b1 is a proper hex number, rather than the binary number 1,
    # we repeat code here and tweak for hex.  Only 0X and 0x should be removed.
    s = self._delete_underscore_strip
    s =~ /^([+-]?)(0[xX])?([[:xdigit:]]*)/
    "16r#{$1}#{$3}"._to_i
  end

  def include?(item)
    !self.index(item).equal?(nil)
  end

  primitive_nobridge '_indexOfByte', 'indexOfByte:startingAt:'

  def index(item, offset=0)
    offset = Type.coerce_to(offset, Integer, :to_int)
    offset += size if offset < 0
    return nil if offset < 0 || offset > size

    if item._isString
      return offset if item.size.equal?(0)
      st_idx = self._findStringStartingAt(item, offset + 1)
    elsif item._isInteger
      return nil if item > 255 || item < 0
      st_idx = self._indexOfByte(item % 256, offset + 1)
    elsif item._isRegexp
      st_idx = item._index_string(self, offset) + 1
    else
      # try to coerce to a number or string and try again,
      #   will raise TypeError if item is a Symbol .
      coerced = Type.coerce_to_string_or_integer(item)
      return self.index(coerced, offset)
    end

    return st_idx.equal?(0) ? nil : st_idx - 1
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
  primitive '_lstrip!', '_removeLeadingSeparators' # in .mcz
  def lstrip!
    raise TypeError, "can't modify frozen string" if frozen?
    _lstrip!
  end

  def match(pattern)
    if pattern._isRegexp
      regexp = pattern
    elsif pattern._isString
      regexp = Regexp.new(pattern)
    else
      begin
        regexp = Regexp.new(pattern.to_str)
      rescue StandardError
          raise TypeError, "wrong argument type #{pattern.class} (expected Regexp)"
      end
    end
    regexp._match_vcglobals(self, 0x30)
  end

  # MNI: next
  # MNI: next!

  def oct
    base, s = self.extract_base(8)
    "#{base}r#{s}"._to_i
  end

  primitive 'replace', '_rubyReplace:'

  primitive          'reverse', 'reverse'

  primitive_nobridge '_reverse_from', '_reverseFrom:'

  def reverse!
    raise TypeError, "can't modify frozen string" if frozen?
    self._reverse_from(self) # returns self
  end

  primitive_nobridge '_lastSubstring', 'findLastSubString:startingAt:'
  primitive_nobridge '_indexOfLastByte', 'indexOfLastByte:startingAt:'

  # Return the index of the last occurrence of the given substring,
  # character or pattern in self.  Returns nil if not found.  If the second
  # parameter is present, it specifies the position in the string to end
  # the search -- characters beyond this point will not be considered.
  def rindex(item, original_offset=Undefined)
    my_size = self.size
    if original_offset.equal?(Undefined)
      offset = my_size
      offset = 1 if (offset.equal?(0))
    else
      offset = Type.coerce_to(original_offset, Integer, :to_int)
      offset += my_size if offset < 0
      offset = my_size - 1 if offset >= my_size
      offset = offset + 1  # to one-based
    end

    return nil if offset <= 0

    if item._isString
      if item.size.equal?(0)
        return my_size if (offset >= my_size)
        return (offset <= my_size) ? (offset - 1) : my_size
      end
      st_idx = self._lastSubstring(item, offset)
    elsif item._isInteger
      return nil if item > 255 || item < 0
      st_idx = self._indexOfLastByte(item % 256 , offset)
    elsif item._isRegexp
      st_idx = item._rindex_string(self, offset - 1)
      return nil if st_idx.equal?(nil)
      st_idx += 1
    else
      coerced = Type.coerce_to(item, String, :to_str)
      return self.rindex(coerced, original_offset)
    end

    return st_idx.equal?(0) ? nil : st_idx - 1
  end

  primitive 'rstrip', 'trimTrailingSeparators'
  primitive '_rstrip!', '_removeTrailingSeparators'  # in .mcz
  def rstrip!
    raise TypeError, "can't modify frozen string" if frozen?
    _rstrip!
  end

  # def scan #  implemented in common/string.rb

  primitive 'size', 'size'

  alias slice []

#   def slice!(*args)
#     raise ArgumentError, 'wrong number of arguments'
#   end


  #     str.slice!(fixnum)           => fixnum or nil
  #     str.slice!(fixnum, fixnum)   => new_str or nil
  #     str.slice!(range)            => new_str or nil
  #     str.slice!(regexp)           => new_str or nil
  #     str.slice!(other_str)        => new_str or nil
  #
  #  Deletes the specified portion from <i>str</i>, and returns the portion
  #  deleted. The forms that take a <code>Fixnum</code> will raise an
  #  <code>IndexError</code> if the value is out of range; the <code>Range</code>
  #  form will raise a <code>RangeError</code>, and the <code>Regexp</code> and
  #  <code>String</code> forms will silently ignore the assignment.
  #
  #     string = "this is a string"
  #     string.slice!(2)        #=> 105
  #     string.slice!(3..6)     #=> " is "
  #     string.slice!(/s.*t/)   #=> "sa st"
  #     string.slice!("r")      #=> "r"
  #     string                  #=> "thing"
  def slice!(start, a_len)
    sz = self.size
    if start._isRegexp
      m_begin, m_len = self._match_regexp(start, a_len)
      return nil if m_begin.equal?(nil)
      raise TypeError, "can't modify frozen string" if self.frozen?
      r = slice!(m_begin, m_len)
      # r.taint if self.tainted? or start.tainted?
      return r
    end

    len = Type.coerce_to(a_len, Integer, :to_int)
    return nil if len < 0
    return '' if len.equal?(0)
    start += sz if start < 0
    return nil if start < 0 || start > sz
    return '' if start.equal?(sz)
    raise TypeError, "can't modify frozen string" if self.frozen?
    s = _at_length(start, len)
    stop = start + len
    stop = sz if stop > sz
    _remove_from_to(start + 1, stop) # convert to smalltalk indexing
    s || ''
  end

  def slice!(arg)
    # Do NOT check for frozen here...fails specs
    if arg._isRegexp
      md = arg.match(self)
      return nil if md.equal?(nil)
      raise TypeError, "can't modify frozen string" if self.frozen?
      start = md.begin(0)
      len = md.end(0) - start
      slice!(start, len)
    elsif arg._isRange
      first, len = arg._beg_len(self.length)
      return nil if first.equal?(nil)
      slice!(first, len)
    elsif arg._isString
      start = self._findStringStartingAt(arg, 1)
      return nil if start.equal?(0)
      slice!(start - 1, arg.length) # adjust coming from smalltalk
    else
      arg = Type.coerce_to(arg, Integer, :to_int)
      s = slice!(arg, 1)
      s[0]
    end
  end

  def _match_regexp(regexp, length)
    md = regexp.match(self)
    return nil if md.equal?(nil)
    idx = Type.coerce_to(length, Integer, :to_int)
    return nil if idx >= md.size or idx < 0
    m_begin = md.begin(idx)
    m_len = md.end(idx) - m_begin
    [m_begin, m_len]
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
    elsif pattern.equal?(nil)
      pattern = /\s+/
    elsif pattern.kind_of?(Regexp)
      # Pass
    else
      pattern = Type.coerce_to(pattern, String, :to_str)
      pattern = Regexp.new(Regexp.quote(pattern))
    end

    start = 0
    ret = []

    last_match = nil

    while match = pattern.match_from(self, start)
      break if limited && limit - ret.size <= 1

      collapsed = match.collapsing?

      if !collapsed || !(match.begin(0).equal?(0))
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

    if ! last_match.equal?(nil)           # GEMSTONE
      pm = last_match.post_match  # GEMSTONE
      ret << (pm.equal?(nil) ? "" : pm)  # GEMSTONE
    elsif ret.empty?
      ret << self.dup
    end

    # Trim from end
    if !ret.empty? and (limit.equal?(0) || limit.equal?(nil) )
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
    # ret = ret.map { |str| str.taint } if self.tainted?

    ret
  end

  def _split_string(string, limit)
    Regexp.new(self)._split_string(string, limit)
  end

  primitive 'squeeze*', 'rubySqueeze:'
  primitive_nobridge 'squeeze', 'rubySqueeze'

  primitive 'squeeze!*', 'rubySqueezeSelf:'
  primitive_nobridge 'squeeze!', 'rubySqueezeSelf'

  def strip
    _strip
  end
  primitive '_strip', 'withBlanksTrimmed'

  def strip!
    raise TypeError, "can't modify frozen string" if frozen?
    replace(strip)
  end

  # Returns a copy of +str+ with the first occurrence of +pattern+ replaced
  # with either +replacement+ or the value of the block.  See the
  # description of <tt>String#gsub</tt> for a description of the
  # parameters.
  def sub(pattern, replacement)
    replacement = Type.coerce_to(replacement, String, :to_str)
    regex = _get_pattern(pattern, true)

    # If pattern is a string, then do NOT interpret regex special characters.
    # stores into caller's $~
    r = if match = regex._match_vcglobals(self, 0x30)
          _replace_match_with(match, replacement)
        else
          dup
        end
    r = self.class.new(r) unless self._isString
    # r.taint if replacement.tainted? || self.tainted?
    r
  end

  def sub(pattern, &block)
    # $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    regex = _get_pattern(pattern, true)
    r = if match = regex._match_vcglobals(self, 0x30)
          _replace_match_with(match, block.call(match).to_s)
        else
          dup
        end
    r = self.class.new(r) unless self._isString
    # r.taint if self.tainted? || pattern.tainted?
    r
  end

  def sub!(pattern, replacement)
    raise TypeError, "sub!: can't modify frozen string" if frozen?

    regex = _get_pattern(pattern, true)
    # stores into caller's $~
    if match = regex._match_vcglobals(self, 0x30)
      replace(_replace_match_with(match, replacement))
      # self.taint if replacement.tainted?
      self
    else
      nil
    end
  end

  def sub!(pattern, &block)
    # $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    raise TypeError, "sub!: can't modify frozen string" if frozen?

    regex = _get_pattern(pattern, true)
    if match = regex._match_vcglobals(self, 0x30)
      replacement = block.call(match)
      replace(_replace_match_with(match, replacement))
      # self.taint if replacement.tainted?
      self
    else
      nil
    end
  end
  # Do ruby conversions of a string or regexp to regexp.
  # If pattern is a string, then quote regexp special characters.
  # If pattern is neither a Regexp nor a String, try to coerce to string.
  def _get_pattern(pattern, quote = false)
    unless pattern._isString || pattern._isRegexp
      if pattern.respond_to?(:to_str)
        pattern = pattern.to_str
        raise TypeError, "can't convert pattern to string" unless pattern._isString
      else
        raise TypeError, "wrong argument type #{pattern.class} (expected Regexp)"
      end
    end
    pattern = Regexp.quote(pattern) if quote && pattern._isString
    pattern = Regexp.new(pattern) unless pattern._isRegexp
    pattern
  end


  primitive '_succ!', 'rubySucc'
  def succ!
    raise TypeError, "succ!: can't modify frozen string" if self.frozen?
    _succ!
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
    d = self.dup
    d.succ!
    # d.taint if self.tainted?
    d
  end

  alias_method :next, :succ
  alias_method :next!, :succ!

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

  primitive '_swapcase!', 'rubySwapcaseInPlace'

  def swapcase!
    raise TypeError, "can't modify frozen string" if frozen?
    self._swapcase!
  end

  def swapcase
    s = self.dup
    s.swapcase!
    # s.taint if self.tainted?
    s
  end

  primitive '_to_f', 'asFloat'
  def to_f
    s = self._delete_underscore_strip
    s =~ /^([+-]?\d*(\.\d+)?\d*([eE][+-]?\d+)?)/
    f = $1._to_f
    f.nan? ? 0.0 : f
  end

  def to_i(base=10)
    base = Type.coerce_to(base, Integer, :to_int)
    if base.equal?(10)
      str = '10r'
      str << self._delete_underscore_strip
      str._to_i
    else
      raise ArgumentError, "illegal radix #{base}" if base < 0 || base == 1 || base > 36
      self.to_inum(base, false)
    end
  end

  primitive_nobridge '_to_i', 'asInteger'

  # Consider self as an integer and return value given base.
  # This is the rubinius API, but we don't care about the check param
  def to_inum(base, check=false)
    if base == 0
      base, s = self.extract_base
    else
      s = self._delete_underscore_strip
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
  MAGLEV_EXTRACT_BASE_TABLE = {"0b" => 2, "0d" => 10, "0o" => 8, "0x" => 16, "0" => 8 }
  MAGLEV_EXTRACT_BASE_TABLE.freeze
  def extract_base(base=10)
    s = self._delete_underscore_strip
    s =~ /^([+-]?)(0[bdox]?)?(.*)/i
    dtwo = $2
    base = MAGLEV_EXTRACT_BASE_TABLE[ dtwo.downcase] unless dtwo.equal?(nil)
    [ base, "#{$1}#{$3}" ]
  end

  def to_a
    self.empty? ? [] : [self]
  end

  def to_s
    if self.class.equal?(String)
      self
    else
      String.new(self)
    end
  end

  def to_str
    if self.class.equal?(String)
      self
    else
      String.new(self)
    end
  end

  primitive_nobridge 'to_sym', 'asSymbol'

  primitive '_tr!', 'rubyTrFrom:to:'
  #     str.tr!(from_str, to_str)   => str or nil
  #
  #  Translates <i>str</i> in place, using the same rules as
  #  <code>String#tr</code>. Returns <i>str</i>, or <code>nil</code> if no
  #  changes were made.
  def tr!(from_str, to_str)
    raise TypeError, "can't modify frozen string" if frozen?
    from = Type.coerce_to(from_str, String, :to_str)
    to   = Type.coerce_to(to_str,   String, :to_str)
    _tr!(from, to)
  end

  #     str.tr(from_str, to_str)   => new_str
  #
  #  Returns a copy of <i>str</i> with the characters in <i>from_str</i> replaced
  #  by the corresponding characters in <i>to_str</i>. If <i>to_str</i> is
  #  shorter than <i>from_str</i>, it is padded with its last character. Both
  #  strings may use the c1--c2 notation to denote ranges of characters, and
  #  <i>from_str</i> may start with a <code>^</code>, which denotes all
  #  characters except those listed.
  #
  #     "hello".tr('aeiou', '*')    #=> "h*ll*"
  #     "hello".tr('^aeiou', '*')   #=> "*e**o"
  #     "hello".tr('el', 'ip')      #=> "hippo"
  #     "hello".tr('a-y', 'b-z')    #=> "ifmmp"
  def tr(from_str, to_str)
    s = self.dup
    s.tr!(from_str, to_str)
    # s.taint if tainted?
    s
  end

  primitive '_tr_s!', 'rubyTrSqueezeFrom:to:'
  #     str.tr_s!(from_str, to_str)   => str or nil
  #
  #  Performs <code>String#tr_s</code> processing on <i>str</i> in place,
  #  returning <i>str</i>, or <code>nil</code> if no changes were made.
  def tr_s!(from_str, to_str)
    raise TypeError, "tr_s!: can't modify frozen string" if frozen?
    return nil if from_str.empty?
    _tr_s!(from_str, to_str)
  end

  #     str.tr_s(from_str, to_str)   => new_str
  #
  #  Processes a copy of <i>str</i> as described under <code>String#tr</code>,
  #  then removes duplicate characters in regions that were affected by the
  #  translation.
  #
  #     "hello".tr_s('l', 'r')     #=> "hero"
  #     "hello".tr_s('el', '*')    #=> "h*o"
  #     "hello".tr_s('el', 'hx')   #=> "hhxo"
  def tr_s(from_str, to_str)
    from = Type.coerce_to(from_str, String, :to_str)
    to   = Type.coerce_to(to_str,   String, :to_str)
    str = self.dup
    # str.taint if self.tainted?
    str._tr_s!(from, to) || str
  end

  primitive 'unpack', 'rubyUnpack:'

  primitive '_upcase', 'asUppercase'
  def upcase
    r = _upcase
    # r.taint if tainted?
    r
  end

  primitive '_upcase!', 'rubyUpcaseInPlace'
  def upcase!
    raise TypeError, "upcase!: can't modify frozen string" if frozen?
    _upcase!
  end

  # MNI: upto

  # ====== Object
  primitive 'inspect', '_rubyPrintString'

  # dup, clone  inherited from Object

  # ====== Comparable:
  # RxINC: This is a cut-n-paste to get things working for mspec.
  # Need to either overwrite or allow a mixin.

  def >(other)
    o = (self <=> other)
    if o.equal?(nil)
      raise ArgumentError, 'comparision failed'
    end
    o > 0
  end

  def <(other)
    o = (self <=> other)
    if o.equal?(nil)
      raise ArgumentError, 'comparision failed'
    end
    o < 0
  end

  def >=(other)
    o = (self <=> other)
    if o.equal?(nil)
      raise ArgumentError, 'comparision failed'
    end
    o >= 0
  end

  def <=(other)
    o = (self <=> other)
    if o.equal?(nil)
      raise ArgumentError, 'comparision failed'
    end
    o <= 0
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
    padstr = Type.coerce_to(padstr, String, :to_str)
    raise ArgumentError, "zero width padding" if padstr.size.equal?(0)

    width = Type.coerce_to(width, Integer, :to_int) unless width._isFixnum
    sz = size
    if width > sz
      padsize = width - sz
    else
      return dup
    end

    _paddedToWithString(direction, width, padstr)
    # taint if padstr.tainted?
    self
  end
end
