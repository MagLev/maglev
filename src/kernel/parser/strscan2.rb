module MagRp
class RpStringScanner
  # second opening so we can have non-dynamic refs to CTYPE constants

  def initialize(string, the_parser)
    @string = string  # expect caller to pass a frozen string
    sz = string.size
    @limit = sz
    @parser = the_parser

    #  @cbytearray used to provide faster self[n]   access to single
    #  bytes for large (>16Kbyte) strings , and to have single copy in
    #  C memory usable by all Regexp searches during a compilation.
    @cbytearray = CByteArray.with_string(string)

    self.reset
  end

  def peek_ch
    # returns a character value, or nil if at end of source
    @cbytearray[@pos]
  end

  def peek_ahead(count)
    @cbytearray[@pos + count]
  end

  def advance(count)
    # not used to advance across LF chars
    @pos = @pos + count
    @match = nil
  end

  def advance_peek(count)
    # not used to advance across LF chars
    p = @pos + count # advance specified number of chars
    if p < @limit
      ch = @cbytearray[p]
    else
      ch = nil
    end
    @pos = p
    ch    # result is peek of one char, or nil if end of source
  end
 
  def advance_to_eol
    # returns false if at end of source after advance
    p = @pos + 1
    lim = @limit
    str = @cbytearray
    while p < lim
      ch = str[p]
      if ch._equal?( ?\n ) 
        @pos = p
        return true
      end
      p += 1
    end
    @pos = p
    return false
  end

  def skip_vt_white
    # advances @pos to first non-white character at or after @pos
    #  white space is  matched by   /\s|\13/
    # if hit end of source per @limit returns 512 
    # elsif white space was skipped result is ascii value + 256 .
    # else returns ascii value of a character.
    p = @pos
    lim = @limit
    if p >= lim
      return 512
    end
    str = @cbytearray
    types_arr = CTYPES_ARR
    ch = str[p] 
    seen = 0
    while types_arr[ ch ] <= CTYPE_VT_WHITE
      p += 1
      seen = 256
      if p >= lim
        @pos = p
        return 512
      end
      ch = str[p] 
    end
    @pos = p
    ch + seen
  end

  def count_eols(start_ofs, end_ofs)
    # return number of LF chars contained in start_ofs..end_ofs
    # TODO  possibly a new Smalltalk primitive to help this
    n = start_ofs
    str = @cbytearray
    count = 0
    while n <= end_ofs
      if str[n]._equal?( ?\n )
        count += 1
      end
      n += 1
    end
    count 
  end

  def at_equalsCONSEC_DOTS
    # CONSEC_DOTS_STRINGS =   [ '...', '..' ]
    # caller must ensure that @string[@pos] == ?.  
    # if @string[@pos..@pos+2] = '...' then return 0
    # elsif if @string[@pos..@pos+1] = '..' then return 1
    # else return 2 ; end
    str = @cbytearray
    idx = @pos + 1
    if str[idx]._equal?( ?. )
      if str[idx+1]._equal?( ?. )
        return 0  # found '...'
      else
        return 1  # found '..'
      end
    end
    2  
  end

  def at_equalsCONSEC_EQUALS
    # CONSEC_EQUALS_STRINGS = [ '===', '==', '=~', '=>' ]
    # caller must ensure that @string[@pos] == ?=
    str = @cbytearray
    idx = @pos + 1
    ch_two = str[idx]
    if ch_two._equal?( ?= )
      if str[idx+1]._equal?( ?= )
        return 0 # found '==='
      else
        return 1 # found '=='
      end
    elsif ch_two._equal?( ?~ )
      return 2   # found '=~'
    elsif ch_two._equal?( ?> )
      return 3   # found '=>'
    end  
    4
  end 

  def at_equalsCONSEC_OR
    #  CONSEC_OR_STRINGS    =  [ '||=' , '||' , '|=' ]
    # caller must ensure that @string[@pos] == ?|
    str = @cbytearray
    idx = @pos + 1
    ch_two = str[idx]
    if ch_two._equal?( ?| )
      if str[idx + 1]._equal?( ?= )
        return 0  # found  '||='
      else
        return 1  # found '||'
      end
    elsif ch_two._equal?( ?= )
      return 2   # found '|='
    end
    3
  end  
 
  def at_equalsCONSEC_STAR
    # CONSEC_STAR_STRINGS  =  [ '**=' , '**' , '*=' ]
    # caller must ensure that @string[@pos] == ?*
    str = @cbytearray
    idx = @pos + 1
    ch_two = str[idx]
    if ch_two._equal?( ?* )
      if str[idx + 1]._equal?( ?= )
        return 0  # found  '**='
      else
        return 1  # found '**'
      end
    elsif ch_two._equal?( ?= )
      return 2   # found '*='
    end
    3
  end

  def at_equalsCONSEC_LT
    # CONSEC_LT_STRINGS    =  [ '<=>' , '<=' , '<<=' , '<<'  ]
    # caller must ensure that @string[@pos] == ?<
    str = @cbytearray
    idx = @pos + 1
    ch_two = str[idx]
    ch_three = str[idx + 1]
    if ch_two._equal?( ?= )
      if ch_three._equal?( ?> )
        return 0
      else
        return 1
      end
    elsif ch_two._equal?( ?< )
      if ch_three._equal?( ?= )
        return 2
      else
        return 3
      end
    end
    4
  end

  def at_equalsCONSEC_GT 
    #     CONSEC_GT_STRINGS    = [ '>=' , '>>=' , '>>' ]
    # caller must ensure that @string[@pos] == ?>
    str = @cbytearray
    idx = @pos + 1
    ch_two = str[idx]
    if ch_two._equal? ( ?= )
      return 0
    elsif ch_two._equal?( ?> )
      if str[idx + 1]._equal?( ?= )
        return 1
      else
        return 2
      end
    end
    3
  end

  def at_equalsCONSEC_AND
    # CONSEC_AND_STRINGS    = [ '&&=' , '&&' , '&=' ]
    # caller must ensure that @string[@pos] == ?&
    str = @cbytearray
    idx = @pos + 1
    ch_two = str[idx]
    if ch_two._equal? ( ?& )
      if str[idx + 1]._equal?( ?= )
        return 0
      else
        return 1
      end
    elsif ch_two._equal?( ?= )
      return 2
    end
    3
  end

  def type_of_ch(ch)
    CTYPES_ARR[ ch ]
  end

  def peek_is_white
    ch = @cbytearray[@pos]
    CTYPES_ARR[ ch ]._equal?(CTYPE_WHITE)
  end

  def peek_is_white__or_eol
    ch = @cbytearray[@pos]
    if ch._equal?( ?\n )
      return true
    end
    CTYPES_ARR[ ch ]._equal?(CTYPE_WHITE)
  end

  def ch_is_digit(ch)
    CTYPES_ARR[ ch ]._equal?(CTYPE_DIGIT)
  end

  def ch_is_digit_alpha_uscore(ch)
    CTYPES_ARR[ ch ] >= CTYPE_UNDERSCORE
  end

  def ch_is_white(ch)
    CTYPES_ARR[ ch ]._equal?(CTYPE_WHITE)
  end

  def ch_is_white__or_eol(ch)
    if ch._equal?( ?\n )
      return true
    end
    CTYPES_ARR[ ch ]._equal?(CTYPE_WHITE)
  end

  def ch_is_vt_white__or_eol(ch)
    if ch._equal?( ?\n )
      return true
    end
    CTYPES_ARR[ ch ] <= CTYPE_VT_WHITE
  end

  def ch_is_uc_alpha(ch)
    CTYPES_ARR[ ch ]._equal?(CTYPE_UC_ALPHA)
  end

  def ch_is_alpha(ch)
    v = CTYPES_ARR[ ch ]
    v._equal?( CTYPE_UC_ALPHA ) || v._equal?( CTYPE_LC_ALPHA )
  end

  def self.ch_is_uc_alpha(ch)
    CTYPES_ARR[ ch ]._equal?(CTYPE_UC_ALPHA)
  end

  def backup(count)
    new_pos = @pos - count
    if new_pos < 0
      @parser.internal_error('attempt to backup past start of source')
    end
    @pos = new_pos
  end

  def line_for_offset(byte_ofs)
    # for syntax error processing and debugging
    # Returns line number or -1
    if byte_ofs._isFixnum
      byte_ofs = byte_ofs - 1 # to zero based
      ofs = 0
      str = @cbytearray
      lnum = 1
      while ofs <= byte_ofs
	if str[ofs]._equal?( ?\n )
	  lnum += 1 
	end
	ofs += 1
      end
      return lnum
    end
    -1
  end

  def src_line_for_offset(byte_ofs)
    # for syntax error processing and debugging
    # Returns line number or -1
    if byte_ofs._isFixnum
      byte_ofs = byte_ofs - 1 # to zero based
      ofs = 0
      str = @cbytearray
      lnum = 1
      prev_eol_ofs = 0
      while ofs <= byte_ofs
	if str[ofs]._equal?( ?\n )
          prev_eol_ofs = ofs
	  lnum += 1 
	end
	ofs += 1
      end
      lim = @limit
      while ofs < lim
        if (str[ofs]._equal?( ?\n ))
          lstr = @string[prev_eol_ofs + 1, ofs - prev_eol_ofs]
          return "line #{lnum}: #{lstr}" 
        end
        ofs += 1
      end
    end
    "(src_line_for_offset not found)"
  end

end
end # MagRp
