class String

  def to_rx
    Regexp.new(self)
  end

  primitive_nobridge '__copyfrom_to', 'copyFrom:to:'
  primitive_nobridge '__findStringStartingAt', 'findString:startingAt:'
  primitive_nobridge '__md5sum', 'md5sumDigest'     # used by lib file  digest/md5.rb
  primitive_nobridge '__remove_from_to', 'removeFrom:to:'
  class_primitive_nobridge '__withAll', 'withAll:'
  class_primitive_nobridge '__alloc', '_basicNew'
  class_primitive_nobridge '__new', 'new:'

  def self.new(*args)
    # this version gets bridge methods
    len = args.__size
    str = self.__alloc
    str.initialize(*args)
    str
  end

  def self.new(str)
    # implement commonly used variant for performance
    if self._equal?(String)
      if str._isString
        s = __withAll(str)
      else
        s = __alloc
        str = Maglev::Type.coerce_to(str, String, :to_str)
        s.replace(str)
      end
    else
      s = __alloc
    end
    s.initialize(str)
    s
  end

  def self.new()
    # implement commonly used variant for performance
    s = __alloc
    s.initialize
    s
  end

  def initialize(*args, &block)
    # this version gets bridge methods , block is ignored
    len = args.length
    # Do nothing for zero args (return self)
    if len._equal?(1)
      if self.class._equal?(String)
        # do nothing
      else
        str = Maglev::Type.coerce_to(args[0], String, :to_str)
        self.replace(str)
      end
    elsif len > 1
      raise ArgumentError, 'too many args'
    end
    self
  end

  def initialize(str)
    # implement commonly used variant for performance
    if self.class._equal?(String)
      # do nothing
    else
      str = Maglev::Type.coerce_to(str, String, :to_str)
      self.replace(str)
    end
    self
  end

  def initialize(str, &block)
    # implement commonly used variant for performance, ignore block
    if self.class._equal?(String)
      # do nothing
    else
      str = Maglev::Type.coerce_to(str, String, :to_str)
      self.replace(str)
    end
    self
  end


  def initialize
    # implement commonly used variant for performance
    self
  end

  def initialize(&block)
    # implement commonly used variant for performance, ignore block
    self
  end

  primitive   '__basic_dup', '_rubyBasicDup'      # use non-singleton class

  def dup
    res = self.__basic_dup
    res.initialize_copy(self)
    res
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
    n = Maglev::Type.coerce_to(n, Integer, :to_int)
    unless n._isFixnum
      if n._isInteger
        raise RangeError , 'arg exceeds max Fixnum'
      end
    end
    if (n < 0)
      raise ArgumentError , 'arg must be positive'
    end
    str = self.class.__alloc
    if n >= 64
      # optimization to reduce number of iterations for large n
      kstr = self.class.__alloc
      kstr.__append_internal(self)
      k = 1
      klim = n.__divide(16)
      # grow kstr to max of ( 1/16 of result size , 16K bytes)
      while k < klim && kstr.__size < 8000
        kstr.__append_internal(kstr)
        k = k * 2
      end
      while n > k
        str.__append_internal(kstr)
        n -= k
      end
    end
    while n > 0
      str.__append_internal(self)
      n -= 1
    end
    str
  end

  primitive  '+', 'rubyConcatenate:'

  #   note smalltalk addAll:  returns arg, not receiver
  primitive '__append', '_rubyAddAll:'

  def <<(arg)
    __append_internal(arg)
  end

  def __append_internal(arg)
    # raise TypeError, "<<: can't modify frozen string" if self.frozen?
    # frozen checked in __append primitive
    if arg._isFixnum
      # raise TypeError, "<<: #{arg} out of range" if arg < 0 or arg > 255 # in prim
      # range checked in  __append primitive
      other = arg
    else
      other = Maglev::Type.coerce_to(arg, String, :to_str)
    end
    self.__append(other)
    # self.taint if other.tainted?
    self
  end

  primitive_env '<=>',  '_rubyCompare' , ':'

  def __prim_compare_failed(o)
    # invoked from Smalltalk code in _rubyCompare<env>:
    return nil unless o.respond_to?(:to_str) && o.respond_to?(:<=>)
    return nil unless tmp = (o <=> self)
    return -tmp
  end

  primitive_nobridge '__uppercaseAt', 'rubyUpperCaseAt:' # arg is one-based

  primitive 'bytesize', 'size'  # added for 1.8.7

  def bytes(&block)   # added for 1.8.7
    unless block_given?
      return StringByteEnumerator.new(self, :bytes)
    end
    arr = [1]
    broke = false
    ea_res = arr.each { |ignore|
      n = 0
      lim = self.__size
      while n < lim
        ch = self.__at(n)
        broke = true
        block.call( ch )
        broke = false
        n += 1
      end
    }
    if broke
      return ea_res  # the argument block did a break
    end
    self
  end

  def chars(&block) # added for 1.8.7
    # Maglev not yet KCODE aware
    unless block_given?
      return StringCharEnumerator.new(self, :chars)
    end
    arr = [1]
    broke = false
    ea_res = arr.each { |ignore|
      n = 0
      lim = self.__size
      while n < lim
        str = ' '
        str[0] = self.__at(n)
        broke = true
        block.call( str )
        broke = false
        n += 1
      end
    }
    if broke
      return ea_res  # the argument block did a break
    end
    self
  end

  def casecmp(o)
    # case-insensitive version of String#<=>
    unless o._isString
      if o._equal?(nil)
        return nil
      end
      return nil if o._isSymbol
      o = Maglev::Type.coerce_to(o, String, :to_str)
    end
    i = 1
    o_size = o.__size
    lim = size > o_size ? o_size : size # lim is the min
    while i <= lim
      sc = self.__uppercaseAt(i)
      oc = o.__uppercaseAt(i)
      result = sc <=> oc
      return result unless result._equal?(0)
      i += 1
    end
    return size <=> o_size
  end

  primitive_env '==',   '_rubyEqual' , ':'
  #  primitive assumes   nil.respond_to?(:to_str) == false
  #  primitive assumes   a_symbol.respond_to?(:to_str) == false

  primitive_env '===',   '_rubyEqual' , ':'   # === same as == for String

  def __prim_equal_failed(other)
    # invoked from Smalltalk code in _rubyEqual<env>:
    if other.respond_to? :to_str
      other == self  # per specs
    else
      false
    end
  end

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
  def =~(*args, &block)
    # only one-arg call supported. any other invocation
    # will have a bridge method interposed which would
    #   require different args to __storeRubyVcGlobal
    raise ArgumentError, 'expected 1 arg'
  end

  def =~(other)
    # no bridge method for this variant
    # =~ is mostly translated to  :match  Sexpression by parser ...
    if other._isRegexp
      m = other.__search(self, 0, nil)
      m.__storeRubyVcGlobal(0x20) # store into caller's $~
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
  primitive_nobridge_env '__at' , '_rubyAt', ':'

  def __prim_at_failed(index)
    # invoked from prim failure code in _rubyAt<env>:
    if index._isRange
      arr = index.__beg_len(self.__size)
      if arr._equal?(nil)
        nil
      else
        self.__at( arr[0] , arr[1] )
      end
    elsif index._isInteger
      raise ArgumentError, 'String#[index] primitive failed'
    else
      index = Maglev::Type.__coerce_to_Fixnum_to_int(index)
      self.__at(index)
    end
  end

  primitive_nobridge_env '[]' ,         '_rubyAt', ':length:'
  primitive_nobridge_env '__at' , '_rubyAt', ':length:'

  def __prim_at_length_failed(start, length)
    # called from Smalltalk primitive failure code
    if start._isRegexp
      arr = self.__match_regexp(start, length, 0x40) # arr is [m_begin, m_len]
      return nil if arr._equal?(nil)
      # no tainted logic
      self.__at( arr[0] , arr[1] )
    else
      if start._isFixnum
        if length._isFixnum
          raise ArgumentError, 'String#[start,length] primitive failed'
        else
          length = Maglev::Type.__coerce_to_Fixnum_to_int(length)
        end
      else
        start = Maglev::Type.__coerce_to_Fixnum_to_int(start)
        length = Maglev::Type.coerce_to(length, Fixnum, :to_int)
      end
      # no tainted logic
      return nil if length < 0
      self.__at(start, length)
    end
  end

  def []=(*args)
    # This variant gets bridge methods
    na = args.__size
    if na._equal?(2)
      self[args.__at(0)] = args.__at(1)
    elsif na._equal?(3)
      self[args.__at(0), args.__at(1)] = args.__at(2)
    else
      raise ArgumentError, 'expected 2 or 3 args'
    end
  end

  primitive_nobridge_env '[]=',     '_rubyAt', ':put:'
  primitive_nobridge_env '__at_put', '_rubyAt', ':put:'
  # Smalltalk code handles  Regexp and String  first args

  def __prim_at_put_failed(index, value)
    # called from Smalltalk
    if value._isFixnum || value._isString
      # ok
    else
      value = Maglev::Type.__coerce_to_String_to_str( value )
      val_coerced = true
    end
    if index._isFixnum
      unless val_coerced._equal?(true)
        raise IndexError, ('String#[index]=, ' + " index #{index} out of range")
      end
      self.__at_put(index, value)
    elsif index._isRange
      arr = index.__beg_len(self.__size)
      if arr._equal?(nil)
        raise IndexError, ('String#[range]=' + "start out of range for range=#{index}")
      else
        self.__at_length_put( arr[0] , arr[1], value)
      end
    else
      index = Maglev::Type.coerce_to(index, Fixnum, :to_int)
      self.__at_put(index, value)
    end
    # taint if value.tainted?
    value
  end

  primitive_nobridge_env '[]=', '_rubyAt', ':length:put:'
  primitive_nobridge_env '__at_length_put', '_rubyAt', ':length:put:'
  # smalltalk code handles Regexp and Fixnum first args

  def __prim_at_length_put_failed(index, count, value)
    index = Maglev::Type.coerce_to(index, Fixnum, :to_int)
    str_value = Maglev::Type.coerce_to(value, String, :to_str)
    count = Maglev::Type.coerce_to(count, Fixnum, :to_int)
    self.__at_length_put(idx, count, str_value)
    # no taint logic
  end

  # MNI: String#~

  primitive '__capitalize', 'rubyCapitalize'

  def capitalize
    x = __capitalize
    # x.taint if self.tainted?
    x
  end

  def capitalize!
    raise TypeError, "can't modify frozen string" if frozen?
    x = __capitalize
    return nil if x == self
    replace(x)
  end

  def center(width, padstr = " ") # from Rubinius
    centered = self.dup
    centered.justify(width, :center, padstr)
  end

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
    str = self.dup
    res = str.chomp!(separator)
    if res._equal?(nil)
      res = str
    end
    res
  end

  #     str.chomp!(separator=$/)   => str or nil
  #
  #  Modifies <i>str</i> in place as described for <code>String#chomp</code>,
  #  returning <i>str</i>, or <code>nil</code> if no modifications were made.
  def chomp!(sep=$/)
    return nil  if sep._equal?(nil)
    my_size = self.__size
    return nil  if my_size._equal?(0)
    sep = Maglev::Type.coerce_to(sep, String, :to_str)
    if sep == "\n"
      last_ch = self.__at(-1)
      diminish_by = 0
      if last_ch.eql?( ?\n )
        diminish_by += 1 if self.__at(-2).eql?( ?\r ) && my_size > 1
      elsif last_ch.not_eql?( ?\r )
        return nil
      end
      diminish_by += 1
      self.__size=(my_size - diminish_by)
    else
      separator_sz = sep.__size
      if separator_sz._equal?(0)
        sz = my_size
        while sz > 0 && self.__at(sz-1).eql?( ?\n )
          if sz > 1 && self.__at(sz-2).eql?( ?\r )
            sz -= 2
          else
            sz -= 1
          end
        end
        return nil  if sz._equal?( my_size )
        self.__size=(sz)
      else
        sep_size = separator_sz
        sz = my_size
        return nil  if sep_size > sz
        sep_size = -sep_size
        while sep_size < 0
          return nil  if sep.__at(sep_size) != self.__at(sep_size)
          sep_size += 1
        end
        self.__size=(sz - separator_sz)
      end
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
    str = self.class.__withAll(self) # preserve species
    str.chop!
    str
  end

  #     str.chop!   => str or nil
  #
  #  Processes <i>str</i> as for <code>String#chop</code>, returning <i>str</i>,
  #  or <code>nil</code> if <i>str</i> is the empty string.  See also
  #  <code>String#chomp!</code>.
  def chop!
    my_size = self.__size
    if my_size._not_equal?( 0 )
      if self.__at(-1).eql?( ?\n )
        if my_size > 1 && self.__at(-2).eql?( ?\r )
          self.__size=(my_size - 2)
        else
          self.__size=(my_size - 1)
        end
      else
        self.__size=(my_size - 1)
      end
      return self
    end
    return nil # no modification made
  end

  alias concat <<

  # def count(*args); end
  # arg to rubyCount: is expected to be an Array , so declare as 'count*'
  primitive 'count*', 'rubyCount:'

  # MNI: crypt

  primitive 'delete*',  'rubyDelete:'
  primitive 'delete!*', 'rubyDeleteInPlace:'

  # asLowercase is a smalltalk to:do: loop in CharacterCollection
  primitive '__downcase', 'asLowercase'
  primitive '__downcase!', 'rubyDowncaseInPlace'

  def downcase
    s = __downcase
    # s.taint if self.tainted?
    s
  end

  def downcase!
    raise TypeError, "can't modify frozen string" if frozen?
    __downcase!
  end

  primitive '__dumpInto' , 'rubyDumpInto:'

  def dump
    res = self.class.__alloc
    self.__dumpInto(res)
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
    unless block_given?
      return StringEachEnumerator.new(self, :each , a_sep) # for 1.8.7
    end
    if a_sep._equal?(nil)
      block.call(self)
      return self
    end
    sep = Maglev::Type.coerce_to(a_sep, String, :to_str)

    # algorithm replicated in   StringEachEnumerator
    # id = self.__id__
    my_size = self.__size
    sep_size = sep.__size
    newline = sep_size._equal?(0) ?  ?\n  : sep.__at(sep_size - 1)

    last = 0
    i = sep_size
    if sep_size._equal?(1)
      while i < my_size
        if self.__at(i-1)._equal?(newline)
          line = self.__at(last, i-last)
          block.call( line )
          # We don't have a way yet to check if the data was modified...
          # modified? id, my_size
          last = i
        end
        i += 1
      end
    elsif sep_size._equal?(0)
      while i < my_size
        if self.__at(i).eql?( ?\n )
          if self.__at(i+=1).not_eql?( ?\n )
            i += 1
            next
          end
          i += 1 while i < my_size && self.__at(i).eql?( ?\n )
        end
        if i > 0 && self.__at(i-1)._equal?( newline )
          line = self.__at(last, i-last)
          # line.taint if tainted?
          block.call( line )
          # We don't have a way yet to check if the data was modified...
          #modified? id, my_size
          last = i
        end
        i += 1
      end
    else
      i += 1
      while i < my_size
        if self.__at(i-1)._equal?(newline) &&
            (sep_size < 2 || self.__at_equals( i - sep_size + 1, sep))
          line = self.__at(last, i-last)
          # line.taint if tainted?
          block.call( line )
          # We don't have a way yet to check if the data was modified...
          #modified? id, my_size
          last = i
        end
        i += 1
      end
    end
    unless last._equal?(my_size)
      line = self.__at(last, my_size-last+1)
      # line.taint if tainted?
      block.call( line)
    end
    self
  end

  alias each_line each

  def each_byte(&block)
    unless block_given?
      return ArrayEnumerator.new(self, :each_byte) # for 1.8.7
    end
    n = 0
    # Do not cache size before looping.  Specs require
    # us to go to new end when string grows or shrinks in the yield.
    while n < self.__size
      block.call( self.__ordAt(n) )
      n = n + 1
    end
    self
  end

  alias each_char chars   # changed to an alias  for 1.8.7

  primitive 'empty?', 'isEmpty'

  def end_with?(*args)  # added for 1.8.7
    n = 0
    lim = args.__size
    my_siz = self.__size
    while n < lim
      str = args[n]
      begin
        str = Maglev::Type.coerce_to(str, String, :to_str)
        if self.__at_equals(my_siz - str.__size + 1 , str)
          return true
        end
      rescue
        # ignore non-coercable arg
      end
      n += 1
    end
    false
  end

  primitive 'eql?', '='

  def not_eql?(other)
    not self.eql?(other)
  end

  def _gsub_copyfrom_to(from, match_start)
    to = match_start # match_start is zero based
    if to > (sz = self.__size)
      to = sz
    end
    self.__copyfrom_to( from + 1 , to )
  end

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
  #
  # Generic version of gsub, for aliasing
  # BOTH BRANCHES COPIED FROM SPECIALIZED VERSIONS BELOW
  def gsub(regex, replacement=MaglevUndefined, &block)
    if !replacement._equal?(MaglevUndefined)
      __gsub_perform_substitution(regex, replacement)[0]
    elsif block
      __gsub_perform_block_substitution(regex, &block)
    else
      StringGsubEnumerator.new(self, :gsub, regex)
    end
  end

  # specialized version for invocation with block
  # COPY TO ELSE BRANCH OF GENERIC VERSION ABOVE IF CHANGED
  def gsub(regex, &block)
    return StringGsubEnumerator.new(self, :gsub, regex) unless block
    __gsub_perform_block_substitution(regex, &block)
  end

  #-- Returns an array of [newvalue, modified], where modified is true if a
  # substitution was performed.  The old gsub! code tried to compare self
  # == gsub(...) to see is a substitution was performed, but that returned
  # incorrect results for something like "replace the last 's' with an 's'"
  # (which breaks Rails routing...)
  #++
  def gsub(regex, replacement)
    __gsub_perform_substitution(regex, replacement)[0]
  end

  def __gsub_perform_substitution(regex, replacement)
    # 1. phase convert arguments to correct types
    hash = Maglev::Type.__coerce_to_Hash_to_hash_or_nil(replacement)
    replacement = Maglev::Type.coerce_to(replacement, String, :to_str) if hash._equal?(nil)

    modified = false
    # 2. phase: prepare substitution loop
    out = self.class.__alloc
    out.force_encoding(self.encoding) # TODO: if force encoding is implemented this should be tested
    start = 0
    pat = self.__get_pattern(regex, true)
    last_match = nil
    # 3. phase: substitute
    pat.__each_match(self) do |match|
      modified = true
      last_match = match
      # append string between matches
      out.__append_internal(self._gsub_copyfrom_to(start, match.begin(0)))
      if hash
        # replace with hash
        val = hash[match.to_s]
        val = val.to_s unless val.kind_of?(String)
      else
        # replace with string 
        val = replacement.__to_sub_replacement(match)
      end
      val.force_encoding(self.encoding) # TODO: if force encoding is implemented this should be tested
      out.__append_internal(val)
      start = match.end(0)
    end
    # append from last match to end of string
    out.__append_internal(self.__copyfrom_to(start + 1, self.__size))
    last_match.__storeRubyVcGlobal(0x30) # store into caller's $~
    return [out, modified]
  end
  
  def __gsub_perform_block_substitution(regex, &block)
    # if block_given?,
    #  $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    start = 0
    out = self.class.__alloc
    last_match = nil
    self.__get_pattern(regex, true).__each_match_vcgl(self, 0x30) do |match|
      last_match = match
      out.__append_internal(self._gsub_copyfrom_to(start, match.begin(0)))
      saveTilde = block.__fetchRubyVcGlobal(0)
      begin
        block.__setRubyVcGlobal(0, match)
        out.__append_internal(block.call(match.__at(0)).to_s)
      ensure
        block.__setRubyVcGlobal(0, saveTilde)
      end
      start = match.end(0)
    end
    out.__append_internal(self.__copyfrom_to(start + 1, self.__size))
    last_match.__storeRubyVcGlobal(0x30) # store into caller's $~
    out
  end

  # From Rubinius
  def __to_sub_replacement(match)
    index = 0
    result = ""
    lim = self.__size
    while index < lim
      current = index
      while current < lim && self.__at(current) != ?\\
        current += 1
      end
      result << self.__at(index, current - index)
      break if current == lim

      # found backslash escape, looking next
      if current == lim - 1
        result << ?\\ # backslash at end of string
        break
      end
      index = current + 1

      cap = self.__at(index)
      if cap.eql?( ?& )
        result << match.__at(0)
      elsif cap.eql?( ?` )
        result << match.pre_match
      elsif cap.eql?( ?' )
        result << match.post_match
      elsif cap.eql?( ?+ )
        result << match.captures.compact.__at(-1).to_s
      elsif cap >= ?0 && cap <= ?9
        result << match.__at(cap.to_i).to_s
      elsif cap.eql?( ?\\ ) # escaped backslash
        result << '\\'
      else     # unknown escape
        result << '\\'
        result << cap
      end
      index += 1
    end
    return result
  end

  def __replace_match_with(match, replacement, flag=true)
    out = self.class.__alloc
    out.__append_internal(self._gsub_copyfrom_to(0, match.begin(0) ))
    unless replacement._equal?(nil)
      if flag
        out.__append_internal(replacement.__to_sub_replacement(match))
      else
        out.__append_internal(replacement)
      end
    end
    out.__append_internal(self.__copyfrom_to(match.end(0) + 1, self.__size))
    out
  end

  def gsub!(regex, str)
    result, modified = __gsub_perform_substitution(regex, str)
    unless modified
      nil
    else
      replace(result)  # replace detects frozen
    end
  end

  def gsub!(regex, &block)
    return StringGsubEnumerator.new(self, :gsub!, regex) unless block
    out = __gsub_perform_block_substitution(regex, &block)
    if self == out
      nil
    else
      replace(out)  # replace detects frozen
    end
  end

  def __delete_underscore_strip
    str = self
    idx = 1
    idx = str.__indexOfByte( ?_.ord , 1 )
    unless idx._equal?(0)
      str = str.delete('_')
    end
    str.strip
  end

  def __delete_underscore
    str = self
    idx = str.__indexOfByte( ?_.ord , 1 )
    unless idx._equal?(0)
      str = str.delete('_')
    end
    str
  end

  def __zreplace_first_double_underscore
    idx = 0
    dest_idx = nil
    lim = self.__size
    ch = self.__at(idx)
    while idx < lim
      nxt = self.__at(idx + 1)
      if ch.eql?( ?_ ) && nxt.eql?( ?_ )
        str = self.dup
        str[idx] = ?Z
        return str
      end
      idx += 1
      ch = nxt
    end
    self
  end

  def __delete_single_underscores_strip
    str = self.dup
    idx = 0
    dest_idx = nil
    lim = str.__size
    ch = str.__at(idx)
    while idx < lim
      nxt = str.__at(idx + 1)
      if ch.eql?( ?_ ) && nxt.not_eql?( ?_ )
        dest_idx = idx
        break
      end
      idx += 1
      ch = nxt
    end
    while idx < lim
      nxt = str.__at(idx + 1)
      if ch.eql?( ?_ ) && nxt.not_eql?( ?_ )
        # do not include ch in result
      else
        str[dest_idx] = ch
        dest_idx += 1
      end
      idx += 1
      ch = nxt
    end
    if dest_idx._not_equal?(nil)
      str.size=(dest_idx)
    end
    str.strip
  end

  def hex
    # Because 0b1 is a proper hex number, rather than the binary number 1,
    # we repeat code here and tweak for hex.  Only 0X and 0x should be removed.
    s = self.__delete_single_underscores_strip  # for 1.8.7
    s =~ /^([+-]?)(0[xX])?([[:xdigit:]]*)/
    sign_str = $1
    num = Integer.__from_string_radix( $3 , 16)
    if sign_str[0].eql?( ?- )
      num = num * -1
    end
    num
  end

  primitive 'hash' , 'hash'

  def include?(item)
    if item._isFixnum
      item = item % 256
    end
    self.index(item)._not_equal?(nil)
  end

  primitive_nobridge '__indexOfByte', 'indexOfByte:startingAt:'  # one-based offset/result

  def index(item, offset=MaglevUndefined, &block)
    # came here via a bridge from args*
    if offset._equal?(MaglevUndefined)
      self.__index(item, 0, 0x50)
    else
      offset = Maglev::Type.coerce_to(offset, Integer, :to_int)
      self.__index(item, offset, 0x50)
    end
  end

  def index(item)
    # code other variants explicitly so num frames from __index_string
    #   to caller will be constant
    self.__index(item, 0, 0x40)
  end

  def index(item, &block)
    self.__index(item, 0, 0x40)
  end

  def index(item, offset)
    offset = Maglev::Type.coerce_to(offset, Integer, :to_int)
    self.__index(item, offset, 0x40)
  end

  def index(item, offset, &block)
    self.__index(item, offset, 0x40)
  end

  def __index(item, zoffset, vcgl_idx)
    my_size = self.__size
    zoffset += my_size if zoffset < 0
    return nil if zoffset < 0 || zoffset > my_size

    if item._isString
      return zoffset if item.__size._equal?(0)
      st_idx = self.__findStringStartingAt(item, zoffset + 1)
      return st_idx._equal?(0) ? nil : st_idx - 1
    elsif item._isInteger
      return nil if item > 255 || item < 0
      st_idx = self.__indexOfByte(item % 256, zoffset + 1)
      return st_idx._equal?(0) ? nil : st_idx - 1
    elsif item._isRegexp
      idx = item.__index_string(self, zoffset, vcgl_idx)
      return idx
    else
      # try to coerce to a number or string and try again,
      #   will raise TypeError if item is a Symbol .
      coerced = Maglev::Type.__coerce_to_string_or_integer(item)
      return self.index(coerced, zoffset)
    end
  end

  primitive_nobridge '__insertall_at', 'insertAll:at:'

  def insert(index, string)
    # account for smalltalk index
    index = Maglev::Type.coerce_to(index, Integer, :to_int)
    string = Maglev::Type.coerce_to(string, String, :to_str)
    idx = index < 0 ? index + size + 2 : index + 1
    if idx <= 0 || idx > size + 1
      raise IndexError, "index #{index} out of string"
    end
    __insertall_at(string, idx) # Flip order of parameters
    self
  end

  primitive '__as_symbol', 'asSymbol'  # allows zero size Symbols

  primitive 'inspect', '_rubyInspect'

  def intern
    if self.__size._equal?(0)
      raise ArgumentError , 'cannot intern zero sized String'
    end
    if self.__indexOfByte(0, 1)._not_equal?(0)
      raise ArgumentError, 'symbol string may not contain `\\0\' '
    end
    self.__as_symbol
  end
  #  to_sym is aliased to intern, see below

  primitive "_paddedToWithString", "padded:to:withString:"

  def justify(width, direction, padstr=" ")
    # This started off as Rubinius, but was heavily modified since most
    # work is done in smalltalk.
    padstr = Maglev::Type.coerce_to(padstr, String, :to_str)
    raise ArgumentError, "zero width padding" if padstr.__size._equal?(0)

    width = Maglev::Type.coerce_to(width, Integer, :to_int) unless width._isFixnum
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

  primitive 'length', 'size'

  alias lines each  # added for 1.8.7  , String#each goes away in 1.9

  def ljust(width, padstr = " ") # from Rubinius
    justified = dup
    justified.justify(width, :left, padstr)
  end

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
    regexp.__match_vcglobals(self, 0x30)
  end

  # MNI: next
  # MNI: next!

  def oct
    str = self.__zreplace_first_double_underscore.strip  # for 1.8.7
    arr = str.__extract_base(8)
    base = arr.__at(0)
    sign_str = arr.__at(1)
    body = arr.__at(2)
    first_ch = body.__at(0)
    if first_ch.eql?( ?+ ) || first_ch.eql?( ?- )
      return 0  # redundant sign character is not an octal digit
    end
    num = Integer.__from_string_radix(body, base)
    if sign_str[0].eql?( ?- )
      num = num * -1
    end
    num
  end

  def partition(pattern)  # added for 1.8.7
    if pattern._isString
      arg_siz = pattern.__size
      if arg_siz._not_equal?(0)
        st_idx = self.__findStringStartingAt(pattern, 1)
        if st_idx._not_equal?(0)
          z_idx = st_idx - 1
          return [self[0, z_idx ] , self[z_idx , arg_siz] , self[z_idx + arg_siz, self.__size]]
        end
      end
      return [ self.dup , '', '' ]
    elsif pattern._isRegexp
      md = pattern.__search(self, 0, nil)
      md.__storeRubyVcGlobal(0x20) # store into caller's $~
      if md._not_equal?(nil)
        idx = md.begin(0)
        mid = md[0]
        return [ self[0, idx], mid , self[idx + mid.__size, self.__size] ]
      end
      return [ self.dup , '', '' ]
    else
      pstr = Maglev::Type.coerce_to(pattern, String, :to_str)
      return self.partition(pstr)
    end
  end

  def partition(&block)
    # reimplement for 1.8.7, otherwise bridge meths hide the implem in Enumerable.
    left = []
    right = []
    each { |o| block.call(o) ? left.push(o) : right.push(o) }
    return [left, right]
  end

  primitive 'replace', '_rubyReplace:'

  primitive          'reverse', 'reverse'

  primitive_nobridge '__reverse_from', '_reverseFrom:'

  def reverse!
    self.__reverse_from(self) # returns self
  end

  primitive_nobridge '__lastSubstring', 'findLastSubString:startingAt:'
  primitive_nobridge '__indexOfLastByte', 'indexOfLastByte:startingAt:'

  # Return the index of the last occurrence of the given substring,
  # character or pattern in self.  Returns nil if not found.  If the second
  # parameter is present, it specifies the position in the string to end
  # the search -- characters beyond this point will not be considered.

  def rindex(item, offset=MaglevUndefined, &block)
    self.__index(item, offset, 0x50)  # came here via a bridge from args*
  end

  def rindex(item)
    # code other variants explicitly so num frames to __rindex_string known
    __rindex(item, MaglevUndefined, 0x40)
  end
  def rindex(item, &block)
    __rindex(item, MaglevUndefined, 0x40)
  end
  def rindex(item, original_offset, &block)
    __rindex(item, original_offset, 0x40)
  end

  def rindex(item, original_offset)
    __rindex(item, original_offset, 0x40)
  end

  def __rindex(item, original_offset, vcgl_idx)
    my_size = self.__size
    if my_size._equal?(0)
      return nil
    end
    if original_offset._equal?(MaglevUndefined)
      was_undef = true
      zoffset = my_size._equal?(0) ? 0 : my_size
    else
      zoffset = Maglev::Type.coerce_to(original_offset, Integer, :to_int)
      zoffset += my_size if zoffset < 0
    end
    return nil if zoffset < 0

    if item._isString
      zorig = zoffset
      zoffset = my_size - 1 if zoffset >= my_size
      if item.__size._equal?(0)
        if was_undef
          return my_size
        elsif zorig >= my_size
          return my_size
        else
          return zoffset
        end
      end
      st_idx = self.__lastSubstring(item, zoffset + 1)
      return st_idx._equal?(0) ? nil : st_idx - 1
    elsif item._isInteger
      return nil if item > 255 || item < 0
      zoffset = my_size - 1 if zoffset >= my_size
      st_idx = self.__indexOfLastByte(item % 256 , zoffset + 1)
      return st_idx._equal?(0) ? nil : st_idx - 1
    elsif item._isRegexp
      zoffset = my_size  if zoffset > my_size  # allow searching for end of string
      zidx = item.__rindex_string(self, zoffset, vcgl_idx)
      return zidx
    else
      coerced = Maglev::Type.coerce_to(item, String, :to_str)
      return self.rindex(coerced, original_offset)
    end
  end

  def rjust(width, padstr = " ") # from Rubinius
    justified = dup
    justified.justify(width, :right, padstr)
  end

  def rpartition(pattern)  # added for 1.8.7
    my_siz = self.__size
    if pattern._isString
      arg_siz = pattern.__size
      if arg_siz._not_equal?(0)
        st_idx = self.__lastSubstring(pattern, my_siz)
        if st_idx._not_equal?(0)
          z_idx = st_idx - 1
          return [self[0, z_idx], self[z_idx , arg_siz], self[z_idx + arg_siz, self.__size]]
        end
      end
      return [ '', '', self.dup ]
    elsif pattern._isRegexp
      md = pattern.__search(self, my_siz , 0)
      md.__storeRubyVcGlobal(0x20) # store into caller's $~
      if md._not_equal?(nil)
        idx = md.begin(0)
        mid = md[0]
        return [ self[0, idx], mid , self[idx + mid.__size, self.__size] ]
      end
      return [ '', '', self.dup ]
    else
      pstr = Maglev::Type.coerce_to(pattern, String, :to_str)
      return self.rpartition(pstr)
    end
  end

  primitive 'rstrip', '_rubyRstrip'
  primitive 'rstrip!', '_rubyRstripInPlace'  # in .mcz

  # def scan #  implemented in common/string.rb

  primitive 'size', 'size'
  primitive '__size', 'size'

  primitive 'size=', 'size:'  # Note size=() not in MRI
  primitive '__size=', 'size:'

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

  def slice!(a1, a2=MaglevUndefined, &block)
    if a2._equal?(MaglevUndefined)
      self.slice!(a1)
    else
      self.slice!(a1, a2)
    end
  end

  def slice!(start, a_len)
    sz = self.__size
    if start._isRegexp
      arr = self.__match_regexp(start, a_len, 0x30) # arr is [ m_begin, m_len]
      return nil if arr._equal?(nil)
      r = slice!(arr.__at(0), arr.__at(1))
      # r.taint if self.tainted? or start.tainted?
      return r
    end
    start = Maglev::Type.coerce_to(start, Integer, :to_int)
    len = Maglev::Type.coerce_to(a_len, Integer, :to_int)
    return nil if len < 0
    return self.class.__alloc if len._equal?(0)
    start += sz if start < 0
    return nil if start < 0 || start > sz
    return self.class.__alloc if start._equal?(sz)
    #  __remove_from_to will detect frozen if changes would occur
    s = __at(start, len)
    stop = start + len
    stop = sz if stop > sz
    __remove_from_to(start + 1, stop) # convert to smalltalk indexing
    if s._equal?(nil)
      return self.class.__alloc
    end
    s
  end

  def slice!(arg)
    # Do NOT check for frozen here...fails specs
    if arg._isRegexp
      md = arg.__search(self, 0, nil)  # inline Regexp#match to update $~
      md.__storeRubyVcGlobal(0x20) # store into caller's $~
      return nil if md._equal?(nil)
      raise TypeError, "can't modify frozen string" if self.frozen?
      start = md.begin(0)
      len = md.end(0) - start
      slice!(start, len)
    elsif arg._isRange
      first, len = arg.__beg_len(self.__size)
      return nil if first._equal?(nil)
      slice!(first, len)
    elsif arg._isString
      start = self.__findStringStartingAt(arg, 1)
      return nil if start._equal?(0)
      slice!(start - 1, arg.__size) # adjust coming from smalltalk
    else
      arg = Maglev::Type.coerce_to(arg, Integer, :to_int)
      s = slice!(arg, 1)
      return nil if s._equal?(nil)
      s.__at(0)
    end
  end

  def __match_regexp(regexp, length, vcgl_idx)
    md = regexp.match(self)
    md.__storeRubyVcGlobal( vcgl_idx ) # update $~
    return nil if md._equal?(nil)
    idx = Maglev::Type.coerce_to(length, Integer, :to_int)
    return nil if idx >= md.size or idx < 0
    m_begin = md.begin(idx)
    m_len = md.end(idx) - m_begin
    [m_begin, m_len]
  end

  #  call-seq:
  #     str.split(pattern=$;, [limit])   => anArray
  #
  #  Divides <i>str</i> into substrings based on a delimiter, returning an array
  #  of these substrings.
  #
  #  If <i>pattern</i> is a <code>String</code>, then its contents are used as
  #  the delimiter when splitting <i>str</i>. If <i>pattern</i> is a single
  #  space, <i>str</i> is split on whitespace, with leading whitespace and runs
  #  of contiguous whitespace characters ignored.
  #
  #  If <i>pattern</i> is a <code>Regexp</code>, <i>str</i> is divided where the
  #  pattern matches. Whenever the pattern matches a zero-length string,
  #  <i>str</i> is split into individual characters.
  #
  #  If <i>pattern</i> is omitted, the value of <code>$;</code> is used.  If
  #  <code>$;</code> is <code>nil</code> (which is the default), <i>str</i> is
  #  split on whitespace as if ` ' were specified.
  #
  #  If the <i>limit</i> parameter is omitted, trailing null fields are
  #  suppressed. If <i>limit</i> is a positive number, at most that number of
  #  fields will be returned (if <i>limit</i> is <code>1</code>, the entire
  #  string is returned as the only entry in an array). If negative, there is no
  #  limit to the number of fields returned, and trailing null fields are not
  #  suppressed.
  #
  #     " now's  the time".split        #=> ["now's", "the", "time"]
  #     " now's  the time".split(' ')   #=> ["now's", "the", "time"]
  #     " now's  the time".split(/ /)   #=> ["", "now's", "", "the", "time"]
  #     "1, 2.34,56, 7".split(%r{,\s*}) #=> ["1", "2.34", "56", "7"]
  #     "hello".split(//)               #=> ["h", "e", "l", "l", "o"]
  #     "hello".split(//, 3)            #=> ["h", "e", "llo"]
  #     "hi mom".split(%r{\s*})         #=> ["h", "i", "m", "o", "m"]
  #
  #     "mellow yellow".split("ello")   #=> ["m", "w y", "w"]
  #     "1,2,,3,4,,".split(',')         #=> ["1", "2", "", "3", "4"]
  #     "1,2,,3,4,,".split(',', 4)      #=> ["1", "2", "", "3,4,,"]
  #     "1,2,,3,4,,".split(',', -4)     #=> ["1", "2", "", "3", "4", "", ""]
  def split(pattern=nil, limit=MaglevUndefined)
    return [] if self.__size._equal?(0)

    if limit._equal?(MaglevUndefined)
      suppress_trailing_empty = true
      limited = false
      limit = nil
    else
      limit = Maglev::Type.coerce_to(limit, Integer, :to_int)
      return [self.dup] if limit._equal?(1)
      limited = limit > 0 ? true : false
      suppress_trailing_empty = limit._equal?(0)
    end

    pattern ||= ($; || " ")

    result = if pattern == ''
               __split_chars(limit, limited, suppress_trailing_empty)
             elsif  pattern == ' '
               __split_on_contiguous_whitespace(limit, limited, suppress_trailing_empty)
             elsif pattern._isString
               __split_string_on(pattern, limit, limited, suppress_trailing_empty)
             else
               __split_regex(pattern, limit, limited, suppress_trailing_empty)
             end
    result
  end

  primitive '__at_equals', 'at:equals:'  # first arg is one-based offset, no coercion


  def __split_string_on(delim, limit, limited, suppress_trailing_empty)
    results = []
    delim_length = delim.__size

    count = start = current = 0
    num = limited ? limit - 1 : 0
    lim = self.__size

    first_char = delim.__at(0)

    while current < lim
      if self.__at(current).eql?(first_char) and self.__at(current, delim_length).eql?(delim)
        results << self.__at(start, (current - start))
        count += 1
        start = current + delim_length
        current = start
        break if limited and count == num
      else
        current += 1
      end
    end

    results << self.__at(start, (lim-start)) unless limited and count > limit
    if suppress_trailing_empty
      while s = results.last and s.empty?
        results.pop
      end
    end
    results
  end

  def __is_whitespace(char)
    char.eql?( ?\ .ord ) ||
      char.eql?( ?\t.ord) ||
      char.eql?( ?\n.ord) ||
      char.eql?( ?\r.ord) ||
      char.eql?( ?\v.ord)
  end

  # Skip contiguous whitespace starting at index and return the index of
  # the first non-whitespace character.  If the end of the string is white
  # space, then the length of the string is returned (i.e., an index past
  # the end).
  def __skip_contiguous_whitespace(index)
    lim = self.__size
    while(index < lim)
      char = self.__ordAt(index)
      return index unless char <= 32 and __is_whitespace(char)  # \t \n etc. are less than space which is 32
      index += 1
    end
    return index
  end

  def __split_on_contiguous_whitespace(limit, limited, suppress_trailing_empty)
    results = []

    eos = self.__size
    count = 0
    start = current = __skip_contiguous_whitespace(0)
    num = limited ? limit - 1 : 0

    while current < eos
      char = self.__ordAt(current)
      if char <= 32 and __is_whitespace(char)
        results << self.__at(start, (current - start))
        count += 1
        start = __skip_contiguous_whitespace(current)
        current = start
        break if limited and count == num
      else
        current += 1
      end
    end

    last = self.__at(start, (eos-start))
    results << last unless last.empty? and suppress_trailing_empty
    results
  end

  # Split on each character, honoring the limits.
  def __split_chars(limit, limited, suppress_trailing_empty)
    result = []

    # lim will be the number of single characters in the result.  If we are
    # limited, then the last element will be the rest of the string:
    #    'hi!'.split('', 2)   # => ['h', 'i!']
    my_siz = self.__size
    lim = my_siz
    lim = (limit-1) if limited and limit < lim

    index = 0
    while index < lim
      result << self.__at(index, 1)
      index += 1
    end

    result << self.__at(index, (my_siz - index)) if limited
    # self[0,0] returns an instance of the recievier: support for sub-classes
    result << self.__at(0,0) unless suppress_trailing_empty || limited
    result
  end

  def __split_regex(pattern, limit, limited, suppress_trailing_empty)
    unless pattern._isRegexp
      pattern = Maglev::Type.coerce_to(pattern, String, :to_str)
      pattern = Regexp.new(Regexp.quote(pattern))
    end

    start = 0
    ret = []

    last_match = nil

    while match = pattern.match_from(self, start)
      break if limited && limit - ret.__size <= 1

      collapsed = match.collapsing?

      if !collapsed || !(match.begin(0)._equal?(0))
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

    if ! last_match._equal?(nil)
      pm = last_match.post_match
      # self[0,0] returns an instance of the recievier: support for sub-classes
      ret << (pm._equal?(nil) ? self.__at(0,0) : pm)
    elsif ret.empty?
      ret << self.dup
    end

    # Trim from end
    if suppress_trailing_empty
      while s = ret.last and s.empty?
        ret.pop
      end
    end

    # If we are matching the empty string, and we have matches, then
    # we need to tack on the trailing empty string match.
    # self[0,0] returns an instance of the recievier: support for sub-classes
    ret << self.__at(0,0) if ret && limit && limit < 0 && last_match && last_match.collapsing?
    ret = ret.map { |str| self.class.__withAll(str) } if !self.instance_of?(String)
    ret
  end

  primitive 'squeeze*', 'rubySqueeze:'
  primitive_nobridge 'squeeze', 'rubySqueeze'

  primitive 'squeeze!*', 'rubySqueezeSelf:'
  primitive_nobridge 'squeeze!', 'rubySqueezeSelf'

  def start_with?(*args)  # added for 1.8.7
    n = 0
    lim = args.__size
    while n < lim
      str = args[n]
      begin
        str = Maglev::Type.coerce_to(str, String, :to_str)
        if self.__at_equals(1 , str)
          return true
        end
      rescue
        # ignore elements of args not coercable
      end
      n += 1
    end
    false
  end

  def start_with?(string)
    begin
      str = Maglev::Type.coerce_to(string, String, :to_str)
      if self.__at_equals(1 , str)
        return true
      end
    rescue
      # ignore arg not coercable
    end
    false
  end

  primitive 'strip', '_rubyStrip'
  primitive 'strip!', '_rubyStripInPlace'

  # Returns a copy of +str+ with the first occurrence of +pattern+ replaced
  # with either +replacement+ or the value of the block.  See the
  # description of <tt>String#gsub</tt> for a description of the
  # parameters.

  # If we were to implement
  #   def sub(pattern, replacement=MaglevUndefined, &block) ; end
  # to support fully general  send or super() ,
  # would still have problems in that
  # we don't know number of frames up stack to find caller's $~

  def sub(pattern, replacement)
    replacement = Maglev::Type.coerce_to(replacement, String, :to_str)
    regex = self.__get_pattern(pattern, true)

    # If pattern is a string, then do NOT interpret regex special characters.
    # stores into caller's $~
    if (match = regex.__match_vcglobals(self, 0x30))
      __replace_match_with(match, replacement)
    else
      dup
    end
    # r.taint if replacement.tainted? || self.tainted?
  end

  def sub(pattern, &block)
    # $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    regex = self.__get_pattern(pattern, true)
    if (match = regex.__match_vcglobals(self, 0x30))
      res = __replace_match_with(match, block.call(match.__at(0)).to_s)
    else
      res = self.dup
    end
    res
  end

  # If we were to implement
  #   def sub!(pattern, replacement=MaglevUndefined, &block) ; end
  # to support fully general  send or super() ,
  # would still have problems in that
  # we don't know number of frames up stack to find caller's $~

  def sub!(pattern, replacement)
    regex = self.__get_pattern(pattern, true)
    # stores into caller's $~
    if match = regex.__match_vcglobals(self, 0x30)
      replace(__replace_match_with(match, replacement))
      # self.taint if replacement.tainted?
      self
    else
      nil
    end
  end

  def sub!(pattern, &block)
    # $~ and related variables will be valid in block if
    #   blocks's home method and caller's home method are the same
    regex = self.__get_pattern(pattern, true)
    if match = regex.__match_vcglobals(self, 0x30)
      replacement = block.call(match.__at(0))

      # Don't run through __to_sub_replacement for block version See trac 946
      replace(__replace_match_with(match, replacement, false))
      # self.taint if replacement.tainted?
      self
    else
      nil
    end
  end

  # Do ruby conversions of a string or regexp to regexp.
  # If pattern is a string, then quote regexp special characters.
  # If pattern is neither a Regexp nor a String, try to coerce to string.
  def __get_pattern(pattern, quote = false)
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
    lim = self.__size
    power = Maglev::Type.coerce_to(power, Fixnum, :to_int)
    if power <= 0
      while n < lim
        tot = tot + self.__at(n)
        n = n + 1
      end
    else
      mod = (1 << power) - 1
      while n < lim
        tot = tot + self.__at(n)
        tot = tot & mod
        n = n + 1
      end
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

  primitive '__to_f', 'asFloat'

  def to_f
    s = self.__delete_underscore_strip
    s =~ /^([+-]?\d*(\.\d+)?\d*([eE][+-]?\d+)?)/  # ignores trailing non-digits
    f = $1.__to_f
    f.nan? ? 0.0 : f
  end

  def to_i(base=10)
    base = Maglev::Type.coerce_to(base, Integer, :to_int)
    if base._equal?(10)
      str = self
      if self.__at(0).eql?( ?0 ) && self.__at(1).eql?( ?d )
        if self.__at(2).eql?( ?- )
          return 0 # sign must come before base specifier
        end
        str = self.__at(2, self.__size - 2)
      end
      Integer.__from_string_radix(str.strip, 10)
    else
      raise ArgumentError, "illegal radix #{base}" if base < 0 || base == 1 || base > 36
      exp_prefix = nil
      if base._equal?(2)
        exp_prefix = '0b'
      elsif base._equal?(8)
        exp_prefix = '0o'
      elsif base._equal?(16)
        exp_prefix = '0x'
      end
      str = self
      if exp_prefix._not_equal?(nil)
        prefix = self.__at(0,2)
        if prefix == exp_prefix
          if self.__at(2).eql?( ?- )
            return 0 # sign must come before base specifier
          end
          str = self.__at(2, self.__size - 2)
        end
      end
      str.to_inum(base, false)
    end
  end

  # Consider self as an integer and return value given base.
  # From the rubinius API.
  def to_inum(base, check=false)
    if check && self.__at('__')._not_equal?(nil)
      raise ArgumentError, "__ in string, in to_inum"
    end
    str = self.strip
    sign_str = nil
    if base._equal?(0)
      arr = str.__extract_base(10)
      base = arr.__at(0)
      sign_str = arr.__at(1)
      str = arr.__at(2)
      s = str
    else
      s = str
      first_ch = s.__at(0)
      if first_ch.eql?( ?+ ) || first_ch.eql?( ?- )
        s = s.__at(1, s.__size - 1)
      end
    end
    if check
      s = s.downcase.__delete_underscore
      bad = false
      if base._equal?(10)
        bad =  s =~ /[^0-9]/
      elsif base._equal?(8)
        bad =  s =~ /[^0-7]/
      elsif base._equal?(16)
        bad =  s =~ /[^0123456789abcdef]/
      elsif base._equal?(2)
        bad =  s =~ /[^01]/
      else
        raise ArgumentError, "to_inum, unsupported base #{base} "
      end
      if bad
        raise ArgumentError, "to_inum, illegal character for base #{base} in #{self.inspect}"
      end
    end
    num = Integer.__from_string_radix(str, base)
    if sign_str._not_equal?(nil) && sign_str[0].eql?( ?- )
      num = num * -1
    end
    num
  end

  # to_a inherited from Enumerable,  uses each

  def to_s
    if self.class._equal?(String)
      self
    else
      String.new(self)
    end
  end

  def to_str
    if self.class._equal?(String)
      self
    else
      String.new(self)
    end
  end

  alias to_sym intern

  primitive '__tr!', 'rubyTrFrom:to:' # prim detects frozen if would change
  #     str.tr!(from_str, to_str)   => str or nil
  #
  #  Translates <i>str</i> in place, using the same rules as
  #  <code>String#tr</code>. Returns <i>str</i>, or <code>nil</code> if no
  #  changes were made.
  def tr!(from_str, to_str)
    from = Maglev::Type.coerce_to(from_str, String, :to_str)
    to   = Maglev::Type.coerce_to(to_str,   String, :to_str)

    # Make the case for single character replacement more efficient.
    # Avoids creating a translation table.
    if from.__size._equal?(1) && to.__size._equal?(1)
      fchar = from[0]
      tochar = to[0]
      lim = size
      i = 0
      while (i < lim)
        self[i] = tochar if self[i].eql?(fchar)
        i += 1
      end
      self
    else
      __tr!(from, to)
    end
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

  primitive '__tr_s!', 'rubyTrSqueezeFrom:to:'  # prim detects frozen if would change
  #     str.tr_s!(from_str, to_str)   => str or nil
  #
  #  Performs <code>String#tr_s</code> processing on <i>str</i> in place,
  #  returning <i>str</i>, or <code>nil</code> if no changes were made.
  def tr_s!(from_str, to_str)
    return nil if from_str.empty?
    __tr_s!(from_str, to_str)
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
    from = Maglev::Type.coerce_to(from_str, String, :to_str)
    to   = Maglev::Type.coerce_to(to_str,   String, :to_str)
    str = self.dup
    # str.taint if self.tainted?
    str.__tr_s!(from, to) || str
  end

  primitive 'unpack', 'rubyUnpack:'

  primitive 'upcase', 'asUppercase'   # no taint propagation

  primitive 'upcase!', 'rubyUpcaseInPlace'  # prim detects frozen if would change

  primitive '__ordAt', '_rubyOrdAt:'

  def ord()
    self.__ordAt(0)
  end

  def chr()
    self[0]
  end

  def tolower()
    self.ord.tolower
  end

  def toupper()
    self.ord.toupper
  end

  # MNI: upto

  # dup, clone  inherited from Object

  # ====== overrides of methods from Comparable:

  def >(other)
    o = (self <=> other)
    if o._equal?(nil)
      raise ArgumentError, 'comparision failed'
    end
    o > 0
  end

  def <(other)
    o = (self <=> other)
    if o._equal?(nil)
      raise ArgumentError, 'comparision failed'
    end
    o < 0
  end

  def >=(other)
    o = (self <=> other)
    if o._equal?(nil)
      raise ArgumentError, 'comparision failed'
    end
    o >= 0
  end

  def <=(other)
    o = (self <=> other)
    if o._equal?(nil)
      raise ArgumentError, 'comparision failed'
    end
    o <= 0
  end

  def between?(min, max)
    (min <= self) && (self <= max)
  end

  def encode(*args)
    # TODO
    self.dup
  end

  def encode!(*args)
    # TODO
    self
  end

  def encoding
    Encoding::UTF_8
  end

  def force_encoding(encoding)
    #TODO
    self
  end

  def valid_encoding?
    #TODO
    true
  end

end
