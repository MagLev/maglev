class Regexp

  # Regexp characters that need quoting
  META_CHARS =  %w![ ] { } ( ) | - * . \\ ? + ^ $ #!

  primitive 'search', '_search:from:to:'
  primitive 'compile', '_compile:options:'
  primitive 'source', 'source'
  self.class.primitive 'alloc', '_basicNew'

  # Rubinius uses this method as the main interface to their primitives.
  # GemStone keeps the interface, but adjusts impl per the Smalltalk
  # interface.  Both Rubinius and GemStone use Oniguruma as the underlying
  # engine.
  def search_region(str, start, finish, forward) # equiv to MRI's re_search
    if forward
      search(str, start, finish)
    else
      search(str, finish, start)
    end
  end

  def initialize(str, options, lang)
    if options._isInteger   # options.kind_of? Integer
      o = options
    else
      o = options ? 1 : 0
    end
    compile(str, o)
  end

  def match(str)
    return nil unless str && str.length > 0
    $~ = search(str, 0, nil)
=begin
       #needed by racc
       if $~
         $& = $~[0]
         $' = $~.post_match
         $` = $~.pre_match
       end
=end
    return $~
  end

  def =~(str)
    if match(str)
      $~.begin(0)
    end
  end

  # TODO: make this private
  def each_match(str, &block)
    pos = 0
    while(pos < str.length)
      $~ = match = search(str, pos, nil)
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
    if self.source == ""
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

class MatchData
  primitive 'at', 'at:'

  def begin(group)
    at((group*2)+1)
  end

  def end(group)
    at((group*2)+2)
  end

  def string
    @inputString
  end

  def pre_match
    string[0..self.begin(0)-1]
  end

  def post_match
    string[self.begin(0)+self[0].size..-1]
  end

  primitive_nobridge '[]' , '_rubyAt:'
  primitive '[]' , '_rubyAt:length:'

  # Ruby global variables $1..$9 implemented by MatchData(C)>>nthRegexRef:
end
