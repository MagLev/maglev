class Regexp

  # Options for passing to new
  IGNORECASE = 1
  EXTENDED   = 2
  MULTILINE  = 4

  # Regexp characters that need quoting
  META_CHARS = "\n\r\f\t " << '[]{}()|-*.\\?+^$#'
  META_CHARS.freeze
  META_REPL_CHARS = 'nrft '
  META_REPL_CHARS.freeze

  class_primitive_nobridge 'new', 'new:options:lang:'
  class_primitive_nobridge 'new', 'new:options:'
  class_primitive_nobridge 'new', 'new:'

  primitive_nobridge '_search', '_search:from:to:'
  primitive_nobridge '_compile', '_compile:options:'
  primitive_nobridge 'kcode', 'kcode'
  primitive_nobridge 'options', 'options'
  primitive_nobridge 'to_s', 'to_s'

  # class_primitive 'alloc', '_basicNew'

  def self.compile(pattern, options = 0, lang = nil)
    if (pattern._isRegexp)
      options = pattern.options
      lang = nil
      pattern = pattern.source
    end
    unless (options._isFixnum)
      options = options ? IGNORECASE : 0
    end
    self.new(pattern, options, lang)
  end

  # BEGIN RUBINIUS
  # Rubinius uses this method as the main interface to their primitives.
  # GemStone keeps the interface, but adjusts impl per the Smalltalk
  # interface.  Both Rubinius and GemStone use Oniguruma as the underlying
  # engine.
  def search_region(str, start, finish, forward) # equiv to MRI's re_search
    if forward
      _search(str, start, finish)
    else
      _search(str, finish, start)
    end
  end

  def match_from(str, count)
    return nil if str.nil? || count >= str.size
    search_region(str, count, str.size, true)
  end
  # END RUBINIUS

  # Return true if +Regexp::IGNORECASE+ is set on this regexp
  def casefold?
    @options & IGNORECASE != 0
  end

  def initialize(str, options=nil)   # 3rd arg language ignored
    # if options == nil, prim defautls to case insensitive
    _compile(str, options)
  end

  def match(*args, &blk)
    # only one-arg call supported. any other invocation
    # will have a bridge method interposed which would
    #   require different args to _storeRubyVcGlobal
    raise ArgumentError, 'expected 1 arg'
  end

  # This method is here to allow Rubinius common code to call into our handling of $~.
  # the Rubinius code does the following to set $~:  Regexp.last_match  = ....
  def self.last_match=(m)
    raise ArgumentError, "Need MatchData, not #{m.class}" unless m.kind_of?(MatchData)
    m._storeRubyVcGlobal(0x20) # store into caller's $~
  end

  def match(str)
    m = _search(str, 0, nil)
    m._storeRubyVcGlobal(0x20) # store into caller's $~
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

  # DO NOT #  def ~(aRegexp) ; end
  # no definition for  ~  because  uses of   ~ aRegexp
  # are  transformed to  aRegexp =~ $_   by the parser .

  def each_match(str, &block)
    # private, does not update $~
    pos = 0
    while(pos < str.length)
      match = _search(str, pos, nil)
      return unless match
      pos = match.end(0)
      if match.begin(0) == pos
        pos += 1
      else
        block.call(match)
      end
    end
  end

  def all_matches(str)
    matches = []
    each_match(str){|m| matches << m}
    matches
  end

  def ==(otherRegexp)
    if (otherRegexp._isRegexp)
      res = otherRegexp.source == source
      res &&=  otherRegexp.kcode == self.kcode
      res &&=  otherRegexp.casefold? == self.casefold?
      res
    else
      false
    end
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
    # used by String index
    start = offset.nil? ? 0 : offset
    md = self._search(string, start, nil)
    md._storeRubyVcGlobal(0x20)
    md.begin(0) + offset
  end

  def _rindex_string(string, offset)
    md = self._search(string, offset, 0)
    md._storeRubyVcGlobal(0x20)
    return nil if md.equal?(nil)
    return md.begin(0) if md[0].nil?
    match_len = md.end(0) - md.begin(0)
    return md.begin(0) + 1 if match_len == 0
    md.begin(0)
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

  class << self
    alias_method :quote, :escape
  end
end
