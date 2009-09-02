class Regexp

  # Options for passing to new
  IGNORECASE = 1
  EXTENDED   = 2
  MULTILINE  = 4
  ALL_OPTIONS_MASK = 7

  # Regexp characters that need quoting
  META_CHARS = "\n\r\f\t " + '[]{}()|-*.\\?+^$#'
  META_CHARS.freeze
  META_REPL_CHARS = 'nrft '
  META_REPL_CHARS.freeze

  class_primitive_nobridge '_new', 'new:options:lang:'

  primitive_nobridge '_search', '_search:from:to:'
  primitive_nobridge '_matchCbytes_from_limit_string*', '_matchCBytes:from:limit:string:'
  primitive_nobridge '_compile', '_compile:options:'
  primitive_nobridge 'kcode', 'kcode'
  primitive_nobridge 'options', 'options'
  primitive_nobridge 'to_s', 'to_s'

  class_primitive_nobridge 'alloc', '_basicNew'
  class_primitive_nobridge '_opts_from_lang', 'optsFromLang:opts:'

  #     Regexp.new(string [, options [, lang]])       => regexp
  #     Regexp.new(regexp)                            => regexp
  #     Regexp.compile(string [, options [, lang]])   => regexp
  #     Regexp.compile(regexp)                        => regexp
  #
  #  Constructs a new regular expression from <i>pattern</i>, which can be either
  #  a <code>String</code> or a <code>Regexp</code> (in which case that regexp's
  #  options are propagated, and new options may not be specified (a change as of
  #  Ruby 1.8). If <i>options</i> is a <code>Fixnum</code>, it should be one or
  #  more of the constants <code>Regexp::EXTENDED</code>,
  #  <code>Regexp::IGNORECASE</code>, and <code>Regexp::MULTILINE</code>,
  #  <em>or</em>-ed together. Otherwise, if <i>options</i> is not
  #  <code>nil</code>, the regexp will be case insensitive. The <i>lang</i>
  #  parameter enables multibyte support for the regexp: `n', `N' = none, `e',
  #  `E' = EUC, `s', `S' = SJIS, `u', `U' = UTF-8.
  #
  #     r1 = Regexp.new('^a-z+:\\s+\w+')           #=> /^a-z+:\s+\w+/
  #     r2 = Regexp.new('cat', true)               #=> /cat/i
  #     r3 = Regexp.new('dog', Regexp::EXTENDED)   #=> /dog/x
  #     r4 = Regexp.new(r2)                        #=> /cat/i
  #
  def self.new(pattern, options = 0, lang = nil)
    if (pattern._isRegexp)
      opts = pattern.options
      lang = nil
      pattern = pattern.source
    else
      opts = options 
      if opts._isFixnum 
        opts = opts & ALL_OPTIONS_MASK
      end
    end
    r = self.alloc
    r.initialize(pattern, opts, lang)
    r
  end

  # Synonym for <code>Regexp.new</code>.
  def self.compile(pattern, options = 0, lang = nil)
    self.new(pattern, options, lang)
  end

  def ==(otherRegexp)
    if (otherRegexp._isRegexp)
      otherRegexp.source == source && otherRegexp.options == @options
    else
      false
    end
  end

  def hash
    (@source.hash) ^ ((@options & ALL_OPTIONS_MASK) .hash)
  end

  def ===(str)
    if ( str._isString ) # if str.kind_of?(String)
      # inline =~  so as to update callers $~
      m = _search(str, 0, nil)
      m._storeRubyVcGlobal(0x20) # store into caller's $~
      if m
        if m.begin(0)
          return true
        end
      end
    end
    false
  end

  # Return true if +Regexp::IGNORECASE+ is set on this regexp
  def casefold?
    !((@options & IGNORECASE).equal?(0))
  end

  # def inspect  # implemented in common/regex.rb

  def initialize(str, options=nil, lang=nil)   # 3rd arg language ignored
    # if options == nil, prim defautls to case insensitive
    if options.equal?(nil)
      opts = 0
    elsif options._isFixnum
      opts = options 
    elsif options.equal?(false)
      opts = 0
    else
      opts = IGNORECASE
    end
    if lang._not_equal?(nil)
      if lang._isString
        opts = self.class._opts_from_lang(lang, opts)     
      else
        raise ArgumentError , 'regex.initialize lang not a String'
      end
    end
    res = _compile(str, opts)
    if res._not_equal?(self)
      raise RegexpError, (res.to_str)  # error from onig_new
    end
    res
  end

  # BEGIN RUBINIUS
  # Rubinius uses this method as the main interface to their primitives.
  # GemStone keeps the interface, but adjusts impl per the Smalltalk
  # interface.  Both Rubinius and GemStone use Oniguruma as the underlying
  # engine.
  def search_region(str, start, finish, forward) # equiv to MRI's re_search
    if forward
      unless finish._isFixnum
        raise TypeError, 'finish must be a Fixnum'
      end
      _search(str, start, finish)
    else
      unless start._isFixnum
        raise TypeError, 'finish must be a Fixnum'
      end
      _search(str, finish, start)
    end
  end

  def match_from(str, offset)
    # search  str[offset .. str.size-1]
    # does not update caller's $~
    if str.equal?(nil)
      return nil
    end
    sz = str.size
    if (offset >= str.size)
      return nil
    end
    _search(str, offset, sz)
  end

  def match_from_nocheck(str, offset)
    # search  str[offset .. str.size-1]
    # caller has already checked that offset < str.size
    # does not update caller's $~
    _search(str, offset, nil)
  end

  def match_start(str, offset) # equiv to MRI's re_match
    _search(str, offset, true )  # use onig_match()
    # does not update caller's $~
  end

  # END RUBINIUS

  def match(*args, &blk)
    # only one-arg call supported. any other invocation
    # will have a bridge method interposed which would
    #   require different args to _storeRubyVcGlobal
    raise ArgumentError, 'expected 1 arg'
  end

  def match(str)
    m = _search(str, 0, nil)
    m._storeRubyVcGlobal(0x20) # store into caller's $~
    m
  end

  def _match_vcglobals(str, vcglobals_arg)
    # Private , for example, see usage in String.sub
    m = _search(str, 0, nil)
    m._storeRubyVcGlobal(vcglobals_arg) # store into specified $~
    m
  end

  def source
    # return the original string of the pattern
    @source
  end

  def =~(*args, &blk)
    # only one-arg call supported. any other invocation
    # will have a bridge method interposed which would
    #   require different args to _storeRubyVcGlobal
    raise ArgumentError, 'expected 1 arg'
  end

  def =~(str)
    # no bridge method for this variant
    m = _search(str, 0, nil)
    m._storeRubyVcGlobal(0x20) # store into caller's $~
    if (m)
      return m.begin(0)
    end
    m
  end

  def ~(*args, &blk)
    # only zero-arg call supported. any other invocation
    # will have a bridge method interposed which would
    #   require different args to _storeRubyVcGlobal
    raise ArgumentError, 'expected zero args'
  end
  
  def ~
    str = self._getRubyVcGlobal(0x21) # get callers $_
    if str.equal?(nil)
      raise TypeError, 'Regexp#~ , caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    m = _search(str, 0, nil)
    m._storeRubyVcGlobal(0x20) # store into caller's $~
    if (m)
      return m.begin(0)
    end
    m
  end

  # during bootstrap,  send and __send__ get no bridge methods
  def send(sym, str)
    if sym.equal?( :=~ )
      m = _search(str, 0, nil)
      m._storeRubyVcGlobal(0x20) # store into caller's $~
      if (m)
        return m.begin(0)
      end
      m
    elsif sym.equal?(:match)
      return nil unless str && str.length > 0
      m = _search(str, 0, nil)
      m._storeRubyVcGlobal(0x20) # store into caller's $~
      m
    else
      super(sym, str)
    end
  end

  def __send__(sym, str)
    if sym.equal?( :=~ )
      m = _search(str, 0, nil)
      m._storeRubyVcGlobal(0x20) # store into caller's $~
      if (m)
        return m.begin(0)
      end
      m
    elsif sym.equal?(:match)
      return nil unless str && str.length > 0
      m = _search(str, 0, nil)
      m._storeRubyVcGlobal(0x20) # store into caller's $~
      m
    else
      super(sym, str)
    end
  end

 def __each_match(str, &block)
    # Private, does not store into callers $~
    idx = 0
    str_sz = str.length
    while idx <= str_sz  # run to size+1  to match  // regex
      match = _search(str, idx, nil)
      if match
        block.call(match)
        pos = match.end(0)
        if pos.equal?( match.begin(0) )
          pos += 1
        end
        if pos <= idx 
          idx += 1
        else
          idx = pos
        end
      else
        return
      end
    end
  end

 def __each_match_vcgl(str, vcglobals_arg, &block)
    # Private, stores into $~ specified by vcglobals_arg
    pos = 0
    while(pos < str.length)
      match = _search(str, pos, nil)
      if match
        # store into specified $~, in case block references it
        match._storeRubyVcGlobal(vcglobals_arg)
        pos = match.end(0)
        if match.begin(0) == pos
          pos += 1
        else
          block.call(match)
        end
      else
        return
      end
    end
  end

  def all_matches(str)
    matches = []
    __each_match(str){|m| matches << m}
    matches
  end

  def self.escape(str)
    # Modified RUBINIUS code
    quoted = ""
    lim = str.size
    i = 0
    ch_str = ' '
    while i < lim
      ch = str[i]
      m_idx = META_CHARS._indexOfByte(ch, 1)
      if (m_idx > 0)
        if (ch <= 0x20)   # handle \n \r \f \t and " "
          escaped_ch = "\\."
          escaped_ch[1] = META_REPL_CHARS[m_idx - 1]
        else
          escaped_ch = "\\."
          escaped_ch[1] = ch
        end
        quoted << escaped_ch
      else
        quoted << ch
      end
      i += 1
    end
    quoted
  end

  def to_rx
    self
  end

  def _index_string(string, offset)
    # used by String#index only
    start = offset.equal?(nil) ? 0 : offset
    md = self._search(string, start, nil)
    md._storeRubyVcGlobal(0x40)
    return nil if md.equal?(nil)
    md.begin(0) 
  end

  def _rindex_string(string, offset)
    res = nil
    str_size = string.size
    if @source.index('\G')._not_equal?(nil)
      # We are giving wrong answers 
      raise ArgumentError, ' \G not supported yet in String#rindex(regexp)'
    end 
    if offset >= str_size  
      res_md = self._search(string, offset, 0) # search backwards
      if res_md._not_equal?(nil)
        res = res_md.begin(0)
      end
    else 
      res_md = self._search(string, offset, str_size)  # forwards 
      if res_md._not_equal?(nil) 
        ofs = res_md.begin(0)
        if ofs.equal?(offset)
          res = ofs  # exact match at offset , use it
        end
      end
      if res.equal?(nil)
	ofs = 0
	while ofs < offset  # loop forwards to find last match < offset
	  md = self._search(string, ofs, str_size)
	  if md.equal?(nil) 
	    break
	  end
	  ofs = md.begin(0) 
	  if ofs < offset 
	    res = ofs 
	    res_md = md
	  end
	  next_ofs = md.end(0)
	  if next_ofs.equal?(ofs) 
	    next_ofs += 1 
	  end
	  ofs = next_ofs
	end
      end
    end
    res_md._storeRubyVcGlobal(0x40)
    return res
  end

  # TODO: limit is not used....
  def _split_string(string, limit)
    result = []
    if @source == ""
      slim = string.size
      i = 0
      while i < slim
        result[i] = string[i, 1]
        i = i + 1
      end
    else
      start = 0
      self.all_matches(string).each do |match|
        result << string[start...match.begin(0)]
        start = match.end(0)
      end
      if(start < string.length)
        result << string[start...string.length]
      end
    end
    result
  end

  # The AST productions for :nth_ref , :back_ref
  #   produce a direct reference to $~  in the sending method.
  # Sends of last_match, =~ , ~  get an implicit ref to $~ in the AST
  #   that triggers a non-deletable method temp definition in the IR

  def self.last_match(*args)
    raise ArgumentError , 'expected 0 or 1 arg'
  end

  def self.last_match
    # no bridge methods for variants after first
    m = self._getRubyVcGlobal(0x20)
    return m
  end

  def self.last_match(an_int)
    # no bridge methods for variants after first
    m = self._getRubyVcGlobal(0x20)
    if m.equal?(nil)
      return m
    else
      return m[an_int]
    end
  end

  def self.union(*args)
    len = args.length
    if len.equal?(0)
      return /(?!)/
    end
    n = 0
    src = ""
    while n < len
      an_arg = args[n]
      if an_arg._isRegexp
        src << an_arg.to_s
      else
        an_arg = Type.coerce_to(an_arg, String, :to_str)
        src << an_arg
      end 
      if n < len - 1
        src << '|'
      end
      n += 1
    end
    self.new(src)
  end

  class << self
    alias_method :quote, :escape
  end
end
Regexp._freeze_constants

