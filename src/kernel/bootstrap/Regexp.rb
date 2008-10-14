class Regexp

  # Options for passing to new
  IGNORECASE = 1
  EXTENDED   = 2
  MULTILINE  = 4

  # Regexp characters that need quoting
  META_CHARS =  %w![ ] { } ( ) | - * . \\ ? + ^ $ #!

  primitive_nobridge '_search', '_search:from:to:'
  primitive_nobridge '_compile', '_compile:options:'
  primitive_nobridge 'options', 'options'
  # class_primitive 'alloc', '_basicNew'

  def source
    # return the original string of the pattern
    @source
  end

  def self.compile(pattern, options = 0, lang = nil)
    Regexp.new(pattern, options, lang)
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
    options & IGNORECASE != 0
  end

  def initialize(str, options, lang)
    if options._isInteger   # options.kind_of? Integer
      o = options
    else
      o = options ? 1 : 0
    end
    _compile(str, o)
  end

  def match(str)
    return nil unless str && str.length > 0
    # search primitive automatically sets $~
    ret = _search(str, 0, nil)
  end

  def =~(str)
    m = match(str)
    if (m)
      return m.begin(0)
    end
    m
  end

  # TODO: make this private
  def each_match(str, &block)
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

  def ===(str)
    if ( str._isString ) # if str.kind_of?(String)
      if  self.=~(str)
        return true
      end
    end
    return false
  end

  def self.escape(str)
    # Modified RUBINIUS code
    quoted = ""

    lim = str.size
    i = 0
    c = ' '          # setup to convert from ints to single char strings
    while i < lim
      c[0] = str[i]  # convert from int to single char string
      quoted << if META_CHARS.include?(c)
                  "\\#{c}"
                elsif c == "\n"
                  "\\n"
                elsif c == "\r"
                  "\\r"
                elsif c == "\f"
                  "\\f"
                elsif c == "\t"
                  "\\t"
                elsif c == " "
                  "\\ "
                else
                  c
                end
      i += 1
    end
    quoted
  end

  # TODO: Regexp.quote: alias it when class alias supported
  #   class << self
  #     alias_method :quote, :escape
  #   end
  def self.quote(str)
    self.escape str
  end

  def to_rx
    self
  end

  IGNORECASE = 1
  EXTENDED = 2
  MULTILINE = 4

  # Were in String.rb
  def _index_string(string, offset)
    md = self.match(string)
    return nil if md.equal?(nil)
    md.begin(0) + offset
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
end
