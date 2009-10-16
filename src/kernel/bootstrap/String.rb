class String


  def to_rx
    Regexp.new(self)
  end

  primitive 'size=', 'size:'  # Note size=() not in MRI

  primitive_nobridge '_copyfrom_to', 'copyFrom:to:'
  primitive_nobridge '_findStringStartingAt', 'findString:startingAt:'
  primitive_nobridge '_md5sum', 'md5sum'
  primitive_nobridge '_remove_from_to', 'removeFrom:to:'
  class_primitive_nobridge '_withAll', 'withAll:'
  class_primitive_nobridge '_alloc', '_basicNew'

  def self.new(str)
    if self.equal?(String) 
      if str._isString
        s = _withAll(str)
      else
        s = _alloc
        str = Type.coerce_to(str, String, :to_str)
        s.replace(str) 
      end      
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
      str = Type.coerce_to(str, String, :to_str)
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
    if n >= 64 
      kstr = self.class.new 
      kstr << self
      k = 1
      klim = n >> 4
      # grow kstr to max of 1/16 of result size or 16K bytes
      while k < klim && kstr.length < 8000 
        kstr << kstr 
        k = k * 2
      end
      while n > k
        str << kstr
        n -= k
      end   
    end
    while n > 0
      str << self
      n -= 1
    end
    str
  end

  primitive  '+', 'rubyConcatenate:'

  #   note smalltalk addAll:  returns arg, not receiver
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

  primitive_env '<=>',  '_rubyCompare' , ':'

  def _prim_compare_failed(o)
    # invoked from Smalltalk code in _rubyCompare<env>:
    return nil unless o.respond_to?(:to_str) && o.respond_to?(:<=>)
    return nil unless tmp = (o <=> self)
    return -tmp
  end

  primitive_nobridge '_uppercase_at', 'rubyUpperCaseAt:' # arg is one-based

  def casecmp(o)
    # case-insensitive version of String#<=>
    if o._isString
      i = 1
      o_size = o.size
      lim = size > o_size ? o_size : size # lim is the min
      while i <= lim
        sc = self._uppercase_at(i)
        oc = o._uppercase_at(i)
        result = sc <=> oc
        return result unless result.equal?(0)
        i += 1
      end
      return size <=> o_size
    else
      if o.equal?(nil)
        return nil
      end
      # From Rubinius...there are a lot of strange things in how
      # string handles <=>...
      return nil if o._isSymbol
      return nil unless o.respond_to?(:to_str) && o.respond_to?(:casecmp)
      return nil unless tmp = (o casecmp self)
      return -tmp
    end
  end

  primitive_env '==',   '_rubyEqual' , ':'
  #  primitive assumes   nil.respond_to?(:to_str) == false  
  #  primitive assumes   a_symbol.respond_to?(:to_str) == false  

  primitive_env '===',   '_rubyEqual' , ':'   # === same as == for String

  def _prim_equal_failed(other)
    # invoked from Smalltalk code in _rubyEqual<env>:
    if other.respond_to? :to_str
      other == self  # per specs
    else
      false
    end
  end

  primitive 'hash' , 'hash'

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
      other =~ self 
    end
  end

  def [](*args)
    # This variant gets bridge methods
    raise ArgumentError, 'wrong number of arguments'
  end

  primitive_nobridge_env '[]' , '_rubyAt', ':'
  primitive_nobridge_env '_at' , '_rubyAt', ':'
 
  def _prim_at_failed(index)
    # invoked from prim failure code in _rubyAt<env>:
    if index._isRange 
      arr = index._beg_len(self.length)
      if arr.equal?(nil)
        nil
      else
        self._at_length( arr[0] , arr[1] )
      end
    elsif index._isInteger
      raise ArgumentError, 'String#[index] primitive failed' 
    else
      index = Type.coerce_to_Fixnum_to_int(index)
      self._at(index)
    end
  end

  primitive_nobridge_env '[]' ,         '_rubyAt', ':length:'
  primitive_nobridge_env '_at_length' , '_rubyAt', ':length:'

  def _prim_at_length_failed(start, length)
    if start._isRegexp
      arr = self._match_regexp(start, length) # arr is [m_begin, m_len]
      return nil if arr.equal?(nil)
      # no tainted logic
      self._at_length( arr[0] , arr[1] )
    else
      if start._isFixnum
        if length._isFixnum
          raise ArgumentError, 'String#[start,length] primitive failed'
        else 
          length = Type.coerce_to_Fixnum_to_int(length)
        end
      else
        start = Type.coerce_to_Fixnum_to_int(start)
        length = Type.coerce_to(length, Fixnum, :to_int)
      end  
      # no tainted logic
      return nil if length < 0
      self._at_length(start, length)
    end
  end

  def []=(*args)
    # This variant gets bridge methods
    raise ArgumentError, 'wrong number of arguments'
  end

  primitive_nobridge_env '[]=',     '_rubyAt', ':put:'
  primitive_nobridge_env '_at_put', '_rubyAt', ':put:'
  # Smalltalk code handles  Regexp and String  first args

  def _prim_at_put_failed(index, value)
    if value._isFixnum || value._isString
      # ok
    else 
      value = Type.coerce_to_String_to_str( value )
      val_coerced = true
    end
    if index._isFixnum
      unless val_coerced.equal?(true)
        raise IndexError, ('String#[index]=, ' + " index #{index} out of range")
      end
      self._at_put(index, value)
    elsif index._isRange 
      arr = index._beg_len(self.length)
      if arr.equal?(nil)
        raise IndexError, ('String#[range]=' + "start out of range for range=#{index}")
      else
        self._at_length_put( arr[0] , arr[1], value)
      end
    else
      index = Type.coerce_to(index, Fixnum, :to_int)
      self._at_put(index, value)
    end
    # taint if value.tainted?
    value
  end

  primitive_nobridge_env '[]=', '_rubyAt', ':length:put:'
  primitive_nobridge_env '_at_length_put', '_rubyAt', ':length:put:'
  # smalltalk code handles Regexp and Fixnum first args

  def _prim_at_length_put_failed(index, count, value)
    index = Type.coerce_to(index, Fixnum, :to_int)
    str_value = Type.coerce_to(value, String, :to_str)
    count = Type.coerce_to(count, Fixnum, :to_int)
    self._at_length_put(idx, count, str_value)
    # no taint logic
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

  primitive '_atEquals', 'at:equals:'

  #     str.chomp(separator=$/)   => new_str
  #
  #  Returns a new <code>String</code> with the given record separator removed
  #  from the end of <i>str</i> (if present). If <code>$/</code> has not been
  #  changed from the default Ruby record separator, then <code>chomp</code> also
  #  removes carriage return characters (that is it will remove <code>\n</code>,
  #  <code>\r</code>, and <code>\r\n</code>).
  #
  #     "hello".chomp            #=> "hello"
  #     "hello\n".chomp          #=> "hello"
  #     "hello\r\n".chomp        #=> "hello"
  #     "hello\n\r".chomp        #=> "hello\n"
  #     "hello\r".chomp          #=> "hello"
  #     "hello \n there".chomp   #=> "hello \n there"
  #     "hello".chomp("llo")     #=> "he"
  def chomp(separator=$/)
    (str = self.dup).chomp!(separator) || str
  end

  #     str.chomp!(separator=$/)   => str or nil
  #
  #  Modifies <i>str</i> in place as described for <code>String#chomp</code>,
  #  returning <i>str</i>, or <code>nil</code> if no modifications were made.
  def chomp!(sep=$/)
    return if sep.nil? || self.empty?
    sep = Type.coerce_to(sep, String, :to_str)
    my_size = self.size

    if (sep == $/ && sep == "\n") || sep == "\n"
      last_ch = self[-1]
      diminish_by = 0
      if last_ch == ?\n
        diminish_by += 1 if self[-2] == ?\r && my_size > 1
      elsif last_ch != ?\r
        return
      end
      diminish_by += 1
      self.size=(my_size - diminish_by)
    elsif sep.size == 0
      size = my_size
      while size > 0 && self[size-1] == ?\n
        if size > 1 && self[size-2] == ?\r
          size -= 2
        else
          size -= 1
        end
      end
      return if size == my_size
      self.size=(size)
    else
      sep_size = sep.size
      size = my_size
      return if sep_size > size
      sep_size = -sep_size
      while sep_size < 0
        return if sep[sep_size] != self[sep_size]
        sep_size += 1
      end
      self.size=(size - sep.size)
    end
    self
  end


  #  call-seq:
  #     str.chop   => new_str
  #
  #  Returns a new <code>String</code> with the last character removed.  If the
  #  string ends with <code>\r\n</code>, both characters are removed. Applying
  #  <code>chop</code> to an empty string returns an empty
  #  string. <code>String#chomp</code> is often a safer alternative, as it leaves
  #  the string unchanged if it doesn't end in a record separator.
  #
  #     "string\r\n".chop   #=> "string"
  #     "string\n\r".chop   #=> "string\n"
  #     "string\n".chop     #=> "string"
  #     "string".chop       #=> "strin"
  #     "x".chop.chop       #=> ""
  def chop
    str = self.class.new(self) # preserve species
    str.chop!
    str
  end

  #     str.chop!   => str or nil
  #
  #  Processes <i>str</i> as for <code>String#chop</code>, returning <i>str</i>,
  #  or <code>nil</code> if <i>str</i> is the empty string.  See also
  #  <code>String#chomp!</code>.
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
    newline = ssize.equal?(0) ?  ?\n  : sep[ssize-1]

    last = 0
    i = ssize
    if ssize.equal?(0)
      while i < my_size
	if self[i].equal?( ?\n )
	  if self[i+=1]._not_equal?( ?\n )
	    i += 1
	    next
	  end
	  i += 1 while i < my_size && self[i].equal?( ?\n )
	end

	if i > 0 && self[i-1].equal?( newline ) &&
	    (ssize < 2 || self._compare_substring(sep, i-ssize, ssize).equal?(0) )
	  line = self[last, i-last]
	  # line.taint if tainted?
	  yield line
	  # We don't have a way yet to check if the data was modified...
	  #modified? id, my_size
	  last = i
	end

	i += 1
      end
    else
      while i < my_size
	if i > 0 && self[i-1].equal?(newline) &&
	    (ssize < 2 || self._compare_substring(sep, i-ssize, ssize).equal?(0))
	  line = self[last, i-last]
	  # line.taint if tainted?
	  yield line
	  # We don't have a way yet to check if the data was modified...
	  #modified? id, my_size
	  last = i
	end
	i += 1
      end
    end
    unless last.equal?(my_size)
      line = self[last, my_size-last+1]
      # line.taint if tainted?
      yield line
    end

    self
  end
  alias each_line each

  def _compare_substring(other, start, size)
    my_size = self.size
    if start > my_size || start + my_size < 0
      raise IndexError, "index #{start} out of string"
    end
    self <=> other[start, size]
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
  #   "hello".gsub(/[aeiou]/, '*')              #=> "h*ll*"
  #   "hello".gsub(/([aeiou])/, '<\1>')         #=> "h<e>ll<o>"
  #   "hello".gsub(/./) {|s| s[0].to_s + ' '}   #=> "104 101 108 108 111 "

  def  _gsub_copyfrom_to(from, match_start)
    to = match_start # match_start is zero based
    if to > (sz = self.size)
      to = sz
    end
    self._copyfrom_to( from + 1 , to )
  end

  def gsub(regex, str)
    str = Type.coerce_to(str, String, :to_str)
    out = self.class.new
    start = 0
    pat = self._get_pattern(regex, true)
    last_match = nil
    pat.__each_match(self) do |match|
      last_match = match
      out << self._gsub_copyfrom_to(start, match.begin(0))
      out << str._to_sub_replacement(match)
      start = match.end(0) 
    end
    out << self._copyfrom_to(start + 1, self.length)
    last_match._storeRubyVcGlobal(0x20) # store into caller's $~ 
    out
  end

  # From Rubinius
  def _to_sub_replacement(match)
    index = 0
    result = ""
    lim = self.size
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

      cap = self[index]
      if cap.equal?( ?& )
          result << match[0]
      elsif cap.equal?( ?` )
          result << match.pre_match
      elsif cap.equal?( ?' )
          result << match.post_match
      elsif cap.equal?( ?+ )
          result << match.captures.compact[-1].to_s
      elsif cap >= ?0 && cap <= ?9 
          result << match[cap - ?0 ].to_s
      elsif cap.equal?( ?\\ ) # escaped backslash
          result << '\\'
      else     # unknown escape
          result << '\\' 
          result << cap
      end
      index += 1
    end
    return result
  end

  def _replace_match_with(match, replacement)
    out = self.class.new
    out << self._gsub_copyfrom_to(0, match.begin(0) )
    unless replacement.equal?(nil)
      out << replacement._to_sub_replacement(match)
    end
    out << self._copyfrom_to(match.end(0) + 1, self.length)
    out
  end


  def gsub(regex, &block)
    # $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    start = 0
    out = self.class.new
    last_match = nil
    self._get_pattern(regex, true).__each_match_vcgl(self, 0x30) do |match|
      last_match = match
      out << self._gsub_copyfrom_to(start, match.begin(0))
      saveTilde = block._fetchRubyVcGlobal(0);
      begin
        block._setRubyVcGlobal(0, match);
        out << block.call(match[0]).to_s
      ensure
        block._setRubyVcGlobal(0, saveTilde);
      end
      start = match.end(0) 
    end
    out << self._copyfrom_to(start + 1, self.length)
    last_match._storeRubyVcGlobal(0x20) # store into caller's $~ 
    out
  end

  # From Rubinius

  def gsub!(regex, str)
    nval = gsub(regex, str)
    if self == nval 
      nil
    else
      replace(nval)  # replace detects frozen
    end
  end

  def gsub!(regex, &block)
    # $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    start = 0
    out = self.class.new
    self._get_pattern(regex, true).__each_match_vcgl(self, 0x30) do |match|
      out << self._gsub_copyfrom_to(start, match.begin(0) )
      saveTilde = block._fetchRubyVcGlobal(0);
      begin
        block._setRubyVcGlobal(0, match);
        out << block.call(match[0]).to_s
      ensure
        block._setRubyVcGlobal(0, saveTilde);
      end
      start = match.end(0) 
    end
    out << self._copyfrom_to(start + 1, self.length)
    if self == out
      nil
    else
      replace(out)  # replace detects frozen
    end
  end

  def _delete_underscore_strip
    str = self
    idx = str._indexOfByte( ?_ , 1 )
    unless idx.equal?(0)
      str = str.delete('_')
    end
    str.strip
  end

  def _delete_underscore
    str = self
    idx = str._indexOfByte( ?_ , 1 )
    unless idx.equal?(0)
      str = str.delete('_')
    end
    str
  end

  def hex
    # Because 0b1 is a proper hex number, rather than the binary number 1,
    # we repeat code here and tweak for hex.  Only 0X and 0x should be removed.
    s = self._delete_underscore_strip
    s =~ /^([+-]?)(0[xX])?([[:xdigit:]]*)/
    Integer._from_string( "16r#{$1}#{$3}" )
  end

  def include?(item)
    if item._isFixnum
      item = item % 256
    end
    self.index(item)._not_equal?(nil)
  end

  primitive_nobridge '_indexOfByte', 'indexOfByte:startingAt:'

  def index(item, offset)
    zoffset = Type.coerce_to(offset, Integer, :to_int)
    self._index(item, zoffset)
  end

  def index(item)
    # code other variants explicitly so num frames from _index_string
    #   to caller will be constant
    self._index(item, 0)
  end
  def index(item, &block)
    self._index(item, 0)
  end
  def index(item, offset, &block)
    self._index(item, offset)
  end

  def _index(item, zoffset)
    my_size = self.size
    zoffset += my_size if zoffset < 0
    return nil if zoffset < 0 || zoffset > my_size

    if item._isString
      return zoffset if item.size.equal?(0)
      st_idx = self._findStringStartingAt(item, zoffset + 1)
      return st_idx.equal?(0) ? nil : st_idx - 1
    elsif item._isInteger
      return nil if item > 255 || item < 0
      st_idx = self._indexOfByte(item % 256, zoffset + 1)
      return st_idx.equal?(0) ? nil : st_idx - 1
    elsif item._isRegexp
      idx = item._index_string(self, zoffset)
      return idx
    else
      # try to coerce to a number or string and try again,
      #   will raise TypeError if item is a Symbol .
      coerced = Type.coerce_to_string_or_integer(item)
      return self.index(coerced, zoffset)
    end
  end

  primitive_nobridge '_insertall_at', 'insertAll:at:'

  def insert(index, string)
    # account for smalltalk index
    index = Type.coerce_to(index, Integer, :to_int)
    string = Type.coerce_to(string, String, :to_str)
    idx = index < 0 ? index + size + 2 : index + 1
    if idx <= 0 || idx > size + 1
      raise IndexError, "index #{index} out of string" 
    end
    _insertall_at(string, idx) # Flip order of parameters
    self
  end

  primitive '_as_symbol', 'asSymbol'  # allows zero size Symbols

  def intern
    if self.size.equal?(0)
      raise ArgumentError , 'cannot intern zero sized String'
    end
    self._as_symbol
  end
  #  to_sym is aliased to intern, see below

  primitive 'length', 'size'

  primitive 'lstrip', '_rubyLstrip'

  primitive 'lstrip!', '_rubyLstripInPlace' # in .mcz

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
    arr = self.extract_base(8)
    base = arr[0]
    str = arr[1]
    s = base.to_s
    s << ?r 
    s << str
    Integer._from_string(s)
  end

  primitive 'replace', '_rubyReplace:'

  primitive          'reverse', 'reverse'

  primitive_nobridge '_reverse_from', '_reverseFrom:'

  def reverse!
    self._reverse_from(self) # returns self
  end

  primitive_nobridge '_lastSubstring', 'findLastSubString:startingAt:'
  primitive_nobridge '_indexOfLastByte', 'indexOfLastByte:startingAt:'

  # Return the index of the last occurrence of the given substring,
  # character or pattern in self.  Returns nil if not found.  If the second
  # parameter is present, it specifies the position in the string to end
  # the search -- characters beyond this point will not be considered.

  def rindex(item, original_offset)
    _rindex(item, original_offset)
  end
  def rindex(item)
    # code other variants explicitly so num frames from _rindex_string
    #   to caller will be constant
    _rindex(item, Undefined)
  end
  def rindex(item, &block)
    _rindex(item, Undefined)
  end
  def rindex(item, original_offset, &block)
    _rindex(item, original_offset)
  end

  def _rindex(item, original_offset)
    my_size = self.size
    if my_size.equal?(0)
      return nil
    end
    if original_offset.equal?(Undefined)
      was_undef = true
      zoffset = my_size.equal?(0) ? 0 : my_size 
    else
      zoffset = Type.coerce_to(original_offset, Integer, :to_int)
      zoffset += my_size if zoffset < 0
    end
    return nil if zoffset < 0

    if item._isString
      zorig = zoffset
      zoffset = my_size - 1 if zoffset >= my_size
      if item.size.equal?(0)
        if was_undef
          return my_size
        elsif zorig >= my_size 
          return my_size 
        else
          return zoffset
        end
      end
      st_idx = self._lastSubstring(item, zoffset + 1)
      return st_idx.equal?(0) ? nil : st_idx - 1
    elsif item._isInteger
      return nil if item > 255 || item < 0
      zoffset = my_size - 1 if zoffset >= my_size
      st_idx = self._indexOfLastByte(item % 256 , zoffset + 1)
      return st_idx.equal?(0) ? nil : st_idx - 1
    elsif item._isRegexp
      zoffset = my_size  if zoffset > my_size  # allow searching for end of string
      zidx = item._rindex_string(self, zoffset)
      return zidx
    else
      coerced = Type.coerce_to(item, String, :to_str)
      return self.rindex(coerced, original_offset)
    end
  end

  primitive 'rstrip', '_rubyRstrip'
  primitive 'rstrip!', '_rubyRstripInPlace'  # in .mcz

  # def scan #  implemented in common/string.rb

  primitive 'size', 'size'

  alias slice []

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
      arr = self._match_regexp(start, a_len) # arr is [ m_begin, m_len]
      return nil if arr.equal?(nil)
      r = slice!(arr[0], arr[1])
      # r.taint if self.tainted? or start.tainted?
      return r
    end
    start = Type.coerce_to(start, Integer, :to_int)
    len = Type.coerce_to(a_len, Integer, :to_int)
    return nil if len < 0
    return self.class.new if len.equal?(0)
    start += sz if start < 0
    return nil if start < 0 || start > sz
    return self.class.new if start.equal?(sz)
    #  _remove_from_to will detect frozen if changes would occur
    s = _at_length(start, len)
    stop = start + len
    stop = sz if stop > sz
    _remove_from_to(start + 1, stop) # convert to smalltalk indexing
    if s.equal?(nil)
      return self.class.new
    end
    s 
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
      return nil if s.equal?(nil)
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

  def split(pattern=nil, limit=Undefined)
    # BEGIN RUBINIUS
    return [] if size.equal?(0)

    if limit._not_equal?(Undefined)
      limit = Type.coerce_to(limit, Integer, :to_int)
      if limit > 0
        return [self.dup] if limit == 1
        limited = true
      else
        limited = false
      end
    else
      limited = false
      limit = nil
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

  primitive '_strip', '_trimReturningSelf:' 

  def strip
    self._strip(false)
  end

  def strip!
    res = self._strip(true)
    if res._not_equal?(self)
      self.replace(res)  # replace detects frozen
      self
    else
      nil
    end 
  end

  # Returns a copy of +str+ with the first occurrence of +pattern+ replaced
  # with either +replacement+ or the value of the block.  See the
  # description of <tt>String#gsub</tt> for a description of the
  # parameters.
  def sub(pattern, replacement)
    replacement = Type.coerce_to(replacement, String, :to_str)
    regex = self._get_pattern(pattern, true)

    # If pattern is a string, then do NOT interpret regex special characters.
    # stores into caller's $~
    r = if (match = regex._match_vcglobals(self, 0x30))
          _replace_match_with(match, replacement)
        else
          dup
        end
    # r.taint if replacement.tainted? || self.tainted?
    r
  end

  def sub(pattern, &block)
    # $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    regex = self._get_pattern(pattern, true)
    if (match = regex._match_vcglobals(self, 0x30))
       res = _replace_match_with(match, block.call(match[0]).to_s)
    else
       res = self.dup
    end
    res
  end

  def sub!(pattern, replacement)
    regex = self._get_pattern(pattern, true)
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

    regex = self._get_pattern(pattern, true)
    if match = regex._match_vcglobals(self, 0x30)
      replacement = block.call(match[0])
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

  primitive 'succ!', 'rubySucc' # prim detects frozen if would change

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

  primitive 'swapcase!', 'rubySwapcaseInPlace' # prim detects frozen if would change

  def swapcase
    s = self.dup
    s.swapcase!
    # s.taint if self.tainted?
    s
  end

  primitive '_to_f', 'asFloat'

  def to_f
    s = self._delete_underscore_strip
    s =~ /^([+-]?\d*(\.\d+)?\d*([eE][+-]?\d+)?)/  # ignores trailing non-digits
    f = $1._to_f
    f.nan? ? 0.0 : f
  end

  def to_i(base=10)
    base = Type.coerce_to(base, Integer, :to_int)
    if base.equal?(10)
      str = self
      if self[0].equal?( ?0 ) && self[1].equal?( ?d )
        if self[2].equal?( ?- )
          return 0 # sign must come before base specifier
        end
        str = self[2, self.size - 2]
      end
      radix_str = '10r'   # needed for ruby to_i semantics of 'abc'.to_i
      radix_str << str._delete_underscore_strip
      Integer._from_string(radix_str)
    else
      raise ArgumentError, "illegal radix #{base}" if base < 0 || base == 1 || base > 36
      exp_prefix = nil
      if base.equal?(2)
        exp_prefix = '0b'
      elsif base.equal?(8)
        exp_prefix = '0o'
      elsif base.equal?(16)
        exp_prefix = '0x'
      end
      str = self
      if exp_prefix._not_equal?(nil)
        prefix = self[0,2]
        if prefix == exp_prefix
          if self[2].equal?( ?- )
            return 0 # sign must come before base specifier
          end
          str = self[2, self.size - 2]
        end   
      end
      str.to_inum(base, false)
    end
  end

  # Consider self as an integer and return value given base.
  # From the rubinius API.
  def to_inum(base, check=false)
    if check && self['__']._not_equal?(nil)
      raise ArgumentError, "__ in string, in to_inum"
    end
    if base.equal?(0)
      arr = self.extract_base # includes  _delete_underscore_strip
      base = arr[0]
      str = arr[1]
    else
      str = self._delete_underscore_strip
    end
    if check
      str = str.downcase
      s = str
      first_ch = s[0]
      if first_ch.equal?( ?+ ) || first_ch.equal?( ?- )
        s = s[1, s.length-1]
      end
      bad = false
      if base.equal?(10)
        bad =  s =~ /[^0-9]/ 
      elsif base.equal?(8)
        bad =  s =~ /[^0-7]/
      elsif base.equal?(16)
        bad =  s =~ /[^0123456789abcdef]/
      elsif base.equal?(2)
        bad =  s =~ /[^01]/
      else
        raise ArgumentError, "to_inum, unsupported base #{base} " 
      end
      if bad
        raise ArgumentError, "to_inum, illegal character for base #{base} in #{self.inspect}"
      end
    end
    s = base.to_s
    s << ?r 
    s << str
    Integer._from_string(s)
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

  alias to_sym intern

  primitive '_tr!', 'rubyTrFrom:to:' # prim detects frozen if would change
  #     str.tr!(from_str, to_str)   => str or nil
  #
  #  Translates <i>str</i> in place, using the same rules as
  #  <code>String#tr</code>. Returns <i>str</i>, or <code>nil</code> if no
  #  changes were made.
  def tr!(from_str, to_str)
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

  primitive '_tr_s!', 'rubyTrSqueezeFrom:to:'  # prim detects frozen if would change
  #     str.tr_s!(from_str, to_str)   => str or nil
  #
  #  Performs <code>String#tr_s</code> processing on <i>str</i> in place,
  #  returning <i>str</i>, or <code>nil</code> if no changes were made.
  def tr_s!(from_str, to_str)
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

  primitive 'upcase!', 'rubyUpcaseInPlace'  # prim detects frozen if would change

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
