class Regexp

  # Options for passing to new
  IGNORECASE = 1    # if these constants change, change code in RubyParser(C)>>new_regexp
  EXTENDED   = 2
  MULTILINE  = 4
  ALL_OPTIONS_MASK = 7

  # Regexp characters that need quoting
  META_CHARS = "\n\r\f\t " + '[]{}()|-*.\\?+^$#'
  META_CHARS.freeze
  META_REPL_CHARS = 'nrft '
  META_REPL_CHARS.freeze

  primitive_nobridge '__search', '_search:from:to:'
  primitive_nobridge '__compile', '_compile:options:'
  primitive_nobridge 'kcode', 'kcode'
  primitive_nobridge 'options', 'options'
  primitive_nobridge 'to_s', 'to_s'
  primitive_nobridge '__regex_to_s', '_regex_to_s'

  class_primitive_nobridge 'alloc', '_basicNew'
  class_primitive_nobridge '__opts_from_lang', 'optsFromLang:opts:'

  # parser has additional primitive defs in src/kernel/parser/extras.rb

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
  def self.new(pattern, options = MaglevUndefined, lang = MaglevUndefined)
    # do not pass to initialize more args than incoming non-default args
    uu = MaglevUndefined
    if pattern._isRegexp
      r = self.alloc
      # support sub-classes: call a version of initialize with
      # same arity as call to new
      if options._equal?(MaglevUndefined)
        r.initialize(pattern)
      else
        r.initialize(pattern.source, pattern.options)
      end
    elsif options._equal?(uu)
      r = self.alloc
      r.initialize(pattern)
    else
      opts = options 
      if opts._isFixnum 
        opts = opts & ALL_OPTIONS_MASK
      end
      if lang._equal?(uu)
        r = self.alloc
        r.initialize(pattern, opts)
      else
        r = self.alloc
        r.initialize(pattern, opts, lang)
      end
    end
    r
  end

  def self.new(pattern)
    r = self.alloc
    if (pattern._isRegexp)
      r.initialize(pattern.source, pattern.options)
    else
      r.initialize(pattern)
    end
    r 
  end

  # Synonym for <code>Regexp.new</code>.
  def self.compile(pattern, options = MaglevUndefined, lang = MaglevUndefined)
    self.new(pattern, options, lang);
  end
  def self.compile(pattern, options)
    self.new(pattern, options)
  end
  def self.compile(pattern)
    self.new(pattern)
  end

  def ==(otherRegexp)
    if (otherRegexp._isRegexp)
      otherRegexp.source == source && otherRegexp.options == @_st_options
    else
      false
    end
  end

  def hash
    (@_st_source.hash) ^ ((@_st_options & ALL_OPTIONS_MASK) .hash)
  end

  def ===(str)
    if ( str._isString ) 
      # inline =~  so as to update callers $~
      m = __search(str, 0, nil)
      m.__storeRubyVcGlobal(0x20) # store into caller's $~
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
    !((@_st_options & IGNORECASE)._equal?(0))
  end

  # def inspect  # implemented in common/regex.rb

  def initialize(str, options=nil, lang=nil) 
    # if options == nil, prim defaults to case insensitive
    if options._equal?(nil)
      opts = 0
    elsif options._isFixnum
      opts = options
    elsif options._equal?(false)
      opts = 0
    else
      opts = IGNORECASE
    end
    if lang._not_equal?(nil)
      if lang._isString
        opts = self.class.__opts_from_lang(lang, opts)
      else
        raise ArgumentError , 'regex.initialize lang not a String'
      end
    end
    res = __compile(str, opts)
    if res._not_equal?(self)
      raise RegexpError, (res.to_str)  # error from onig_new
    end
    res
  end

  def initialize(arg)
    res = if arg._isString
            __compile(arg, 0)
          else
            __compile(arg.source, arg.options)
          end
    if res._not_equal?(self)
      raise RegexpError, (res.to_str)  # error from onig_new
    end
    res
  end

  def initialize(string, options)
    if options._isFixnum
      opts = options
    elsif options._equal?(false) || options._equal?(nil)
      opts = 0
    else
      opts = IGNORECASE
    end
    res = __compile(string, opts)
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
      __search(str, start, finish)
    else
      unless start._isFixnum
        raise TypeError, 'finish must be a Fixnum'
      end
      __search(str, finish, start)
    end
  end

  def match_from(str, offset)
    # search  str[offset .. str.size-1]
    # does not update caller's $~
    if str._equal?(nil)
      return nil
    end
    sz = str.size
    if (offset >= str.size)
      return nil
    end
    __search(str, offset, sz)
  end

  def match_from_nocheck(str, offset)
    # search  str[offset .. str.size-1]
    # caller has already checked that offset < str.size
    # does not update caller's $~
    __search(str, offset, nil)
  end

  def match_start(str, offset) # equiv to MRI's re_match
    __search(str, offset, true )  # use onig_match()
    # does not update caller's $~
  end

  # END RUBINIUS

  def match(*args, &block)
    # only one-arg call supported. any other invocation
    # will have a bridge method interposed which would
    #   require different args to __storeRubyVcGlobal
    raise(ArgumentError, 'expected 1 arg with no block') if block
    return self.match(args[0])
  end

  def match(str)
    m = __search(str, 0, nil)
    m.__storeRubyVcGlobal(0x20) # store into caller's $~
    m
  end

  def __match_vcglobals(str, vcglobals_arg)
    # Private , for example, see usage in String.sub
    m = __search(str, 0, nil)
    m.__storeRubyVcGlobal(vcglobals_arg) # store into specified $~
    m
  end

  def source
    # return the original string of the pattern
    @_st_source
  end

  def =~(*args, &block)
    # only one-arg call supported. any other invocation
    # will have a bridge method interposed which would
    #   require different args to __storeRubyVcGlobal
    raise(ArgumentError, 'expected 1 arg with no block') if block
    return self =~ args[0]
  end

  def =~(str)
    # no bridge method for this variant
    str = str.to_str if str.respond_to?(:to_str) unless String === str
    m = __search(str, 0, nil)
    m.__storeRubyVcGlobal(0x20) # store into caller's $~
    if (m)
      return m.begin(0)
    end
    m
  end

  def ~(*args, &block)
    # only zero-arg call supported. any other invocation
    # will have a bridge method interposed which would
    #   require different args to __storeRubyVcGlobal
    raise ArgumentError, 'expected zero args with no block'
  end
  
  def ~
    str = self.__getRubyVcGlobal(0x21) # get callers $_
    if str._equal?(nil)
      raise TypeError, 'Regexp#~ , caller frame has no reference to $_ '
    end
    unless str._isString
      raise TypeError, '$_ is not a String'
    end
    m = __search(str, 0, nil)
    m.__storeRubyVcGlobal(0x20) # store into caller's $~
    if (m)
      return m.begin(0)
    end
    m
  end

  # during bootstrap,  send and __send__ get no bridge methods
  def send(sym, str)
    if sym._equal?( :=~ )
      m = __search(str, 0, nil)
      m.__storeRubyVcGlobal(0x20) # store into caller's $~
      if (m)
        return m.begin(0)
      end
      m
    elsif sym._equal?(:match)
      return nil unless str && str.length > 0
      m = __search(str, 0, nil)
      m.__storeRubyVcGlobal(0x20) # store into caller's $~
      m
    else
      super(sym, str)
    end
  end

  def __send__(sym, str)
    if sym._equal?( :=~ )
      m = __search(str, 0, nil)
      m.__storeRubyVcGlobal(0x20) # store into caller's $~
      if (m)
        return m.begin(0)
      end
      m
    elsif sym._equal?(:match)
      return nil unless str && str.length > 0
      m = __search(str, 0, nil)
      m.__storeRubyVcGlobal(0x20) # store into caller's $~
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
      match = __search(str, idx, nil)
      if match
        block.call(match)
        pos = match.end(0)
        if pos._equal?( match.begin(0) )
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
      match = __search(str, pos, nil)
      if match
        # store into specified $~, in case block references it
        match.__storeRubyVcGlobal(vcglobals_arg)
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
      ch = str[i].ord
      m_idx = META_CHARS.__indexOfByte(ch, 1)
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

  def __index_string(string, offset, vcgl_idx)
    # used by String#index only
    start = offset._equal?(nil) ? 0 : offset
    md = self.__search(string, start, nil)
    md.__storeRubyVcGlobal(0x40)
    return nil if md._equal?(nil)
    md.begin(0)
  end

  def __rindex_string(string, offset, vcgl_idx)
    res = nil
    str_size = string.size
    if @_st_source.index('\G')._not_equal?(nil)
      # We are giving wrong answers
      raise ArgumentError, ' \G not supported yet in String#rindex(regexp)'
    end
    if offset >= str_size
      res_md = self.__search(string, offset, 0) # search backwards
      if res_md._not_equal?(nil)
        res = res_md.begin(0)
      end
    else
      res_md = self.__search(string, offset, str_size)  # forwards
      if res_md._not_equal?(nil)
        ofs = res_md.begin(0)
        if ofs._equal?(offset)
          res = ofs  # exact match at offset , use it
        end
      end
      if res._equal?(nil)
  ofs = 0
  while ofs < offset  # loop forwards to find last match < offset
    md = self.__search(string, ofs, str_size)
    if md._equal?(nil)
      break
    end
    ofs = md.begin(0)
    if ofs < offset
      res = ofs
      res_md = md
    end
    next_ofs = md.end(0)
    if next_ofs._equal?(ofs)
      next_ofs += 1
    end
    ofs = next_ofs
  end
      end
    end
    res_md.__storeRubyVcGlobal(vcgl_idx)
    return res
  end

  # TODO: limit is not used....
  def __split_string(string, limit)
    result = []
    if @_st_source == ""
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
    raise ArgumentError , 'expected 0 or 1 arg with no block'
  end

  def self.last_match
    # no bridge methods for variants after first
    m = self.__getRubyVcGlobal(0x20)
    return m
  end

  def self.last_match(an_int)
    # no bridge methods for variants after first
    m = self.__getRubyVcGlobal(0x20)
    if m._equal?(nil)
      return m
    else
      return m[an_int]
    end
  end

  #     Regexp.union([pattern]*)   => new_str
  #
  #  Return a <code>Regexp</code> object that is the union of the given
  #  <em>pattern</em>s, i.e., will match any of its parts. The <em>pattern</em>s
  #  can be Regexp objects, in which case their options will be preserved, or
  #  Strings. If no arguments are given, returns <code>/(?!)/</code>.
  #
  #     Regexp.union                         #=> /(?!)/
  #     Regexp.union("penzance")             #=> /penzance/
  #     Regexp.union("skiing", "sledding")   #=> /skiing|sledding/
  #     Regexp.union(/dogs/, /cats/i)        #=> /(?-mix:dogs)|(?i-mx:cats)/
  def self.union(*args)
    len = args.length
    if len._equal?(1) 
      arr = args[0]
      if arr._isArray
        args = arr
        len = arr.length
      end
    end
    if len._equal?(0)
      return /(?!)/
    end
    n = 0
    src = ""
    k_cod = nil  # nil or a String
    while n < len
      if n > 0
        src << '|'
      end
      an_arg = args[n]
      if an_arg._isRegexp
        s_kcode = an_arg.kcode
        if k_cod._equal?(nil)
          k_cod = s_kcode
        elsif s_kcode != k_cod
          raise ArgumentError, 'mixed kcode in Regexp.union'
        end
        src << an_arg.to_s
      else
        an_arg = Maglev::Type.coerce_to(an_arg, String, :to_str)
        src << Regexp.escape(an_arg)
      end 
      n += 1
    end
    self.new(src, 0, k_cod)
  end

  class << self
    alias_method :quote, :escape
  end
end
Regexp.__freeze_constants

