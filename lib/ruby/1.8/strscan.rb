# original copy from Rubinius  March 09
#  modified to be Maglev style efficient ruby code.
#  see file  strscan.rb.base  for the original Rubinius code

class ScanError < StandardError; end

class StringScanner
  Id = "bite me $Id".freeze
  Version = "1.0.0".freeze

  # attr_reader :pos, :match, :string
  def pos
    @pos
  end
  def match
    @match
  end
  def string
    @string
  end

  def pos=(n)
    n = Type.coerce_to(n, Fixnum, :to_int)
    raise RangeError, "xxx" if n > @string.size
    @pos = n
  end

  alias :pointer :pos
  alias :pointer= :pos=

  def [](n)
    # coercion will be implicit by match[] primitive
    # n = Type.coerce_to(n, Fixnum, :to_int)
    match[n]
  end

  def bol?
    if pos.equal?(0) 
      return true
    end
    pminus = pos - 1
    string[pminus..pminus] == "\n"
  end
  alias :beginning_of_line? :bol?

  def check( pattern)
    _scan_headonly(pattern, false, true)
  end

  def check_until(pattern)
    _scan(pattern, false, true)
  end

  #def clear
  #  warn "StringScanner#clear is obsolete; use #terminate instead" if $VERBOSE
  #  terminate
  #end

  def concat(str)
    @string << str
    self
  end
  alias :<< :concat # TODO: reverse

  #def empty?
  #  warn "StringScanner#empty? is obsolete; use #eos? instead?" if $VERBOSE
  #  eos?
  #end

  def eos?
    @pos >= @string.size
  end

  def exist?( pattern)
    _scan( pattern, false, false)
  end

  def get_byte
    scan(/./mn)
  end

  #def getbyte
  #  warn "StringScanner#getbyte is obsolete; use #get_byte instead" if $VERBOSE
  #  get_byte
  #end

  def getch
    scan(/./m)
  end

  def initialize(string, dup = false)
    @string = string
    self.reset
  end

  def initialize_copy(orig)
    @match  = orig.match
    @pos    = orig.pos
    s = orig.string
    @string = s
  end

  def inspect
    if defined? @string then
      rest = string.size > 5 ? string[pos..pos+4] + "..." : string
      r = if eos? then
            "#<StringScanner fin>"
          elsif pos > 0 then
            prev = string[0...pos].inspect
            "#<StringScanner #{pos}/#{string.size} #{prev} @ #{rest.inspect}>"
          else
            "#<StringScanner #{pos}/#{string.size} @ #{rest.inspect}>"
          end
      r.taint if self.string.tainted?
      r
    else
      "#<StringScanner (uninitialized)>"
    end
  end

  def match?(pattern)
    _scan_headonly(pattern, false, false)
  end

  def matched
    match.to_s if matched?
  end

  def matched?
    not @match.equal?(nil)
  end

  def matched_size
    m = @match
    m.to_s.size if (not m.equal?(nil))
  end

  #def matchedsize
  #  warn "StringScanner#matchedsize is obsolete; use #matched_size instead" if $VERBOSE
  #  matched_size
  #end

  def post_match
    m = @match
    m.post_match if (not m.equal?(nil))
  end

  def pre_match
    m = @match
    @string[0...(@pos - m.to_s.size)] if (not m.equal?(nil))
  end

  def reset
    @prev_pos = @pos = 0
    @match = nil
    self
  end

  def rest
    @string[@pos..-1]
  end

  def rest?
    @pos < @string.size
  end

  def rest_size
    p = @pos
    ss = @string.size 
    if p < ss
      ss - p
    else
      0
    end
  end

  def restsize
    warn "StringScanner#restsize is obsolete; use #rest_size instead" if $VERBOSE
    rest_size
  end

  def scan( pattern)
    _scan_headonly( pattern, true, true)
  end

  def scan_until( pattern)
    _scan(pattern, true, true)
  end

  def scan_full( pattern, succptr, getstr)
    _scan_headonly( pattern, succptr, getstr)
  end

  def search_full( pattern, succptr, getstr)
    _scan( pattern, succptr, getstr)
  end

  def self.must_C_version
    self
  end

  def skip( pattern)
    _scan_headonly( pattern, true, false)
  end

  def skip_until( pattern)
    _scan( pattern, true, false)
  end

  def string=( s)
    reset
    @string = s
  end

  def terminate
    @match = nil
    @pos = @string.size
    self
  end

  def unscan
    raise ScanError if @match.equal?(nil)
    @pos = @prev_pos
    @prev_pos = nil
    @match = nil
    self
  end

  def peek( len)
    unless len._isFixnum
      raise TypeError,'expected Fixnum'
    end
    raise ArgumentError if len < 0
    return "" if len.equal?(0)
    return @string[@pos, len]
  end

  #def peep len
  #  warn "StringScanner#peep is obsolete; use #peek instead" if $VERBOSE
  #  peek len
  #end

  def _scan( pattern, succptr, getstr)
    if pattern._isString
      # ok
    elsif pattern._isRegexp
      # ok 
    else
      pattern = Type.coerce_to(pattern, String, :to_str)
    end
    @match = nil

    ppos = @pos
    sstr = @string
    return nil if (sstr.size - ppos) < 0 # TODO: make more elegant

    # rest = self.rest
    # @match = pattern.match(rest)

    mmatch = pattern.match_from_nocheck(sstr, ppos)
    @match = mmatch

    return nil unless  mmatch

    m = rest[0, mmatch.end(0)]

    if succptr then
      @prev_pos = ppos
      @pos += m.size
    end

    if getstr then
      m
    else
      m.size
    end
  end
  private :_scan

  def _scan_headonly( pattern, succptr, getstr)
    if pattern._isString
      # ok
    elsif pattern._isRegexp
      # ok 
    else
      pattern = Type.coerce_to(pattern, String, :to_str)
    end
    @match = nil

    ppos = @pos
    sstr = @string
    return nil if (sstr.size - ppos) < 0 # TODO: make more elegant

    # rest = self.rest
    rest = sstr[ppos..-1]

    # NOTE - match_start is an Oniguruma feature that Rubinius exposes.
    # We use it here to avoid creating a new Regexp with '^' prepended.
    mmatch = pattern.match_start(rest, 0)
    @match = mmatch

    return nil unless  mmatch

    m = rest[0, mmatch.end(0)]

    if succptr then
      @prev_pos = ppos
      @pos += m.size
    end

    if getstr then
      m
    else
      m.size
    end
  end

  private :_scan_headonly
end
