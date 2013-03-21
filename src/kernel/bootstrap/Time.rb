# Time in Ruby is identically Smalltalk RubyTime
#
# The Smalltalk RubyTime class holds a single value, @microseconds, which
# is the number of microseconds since the epoch.  

class Time

  ######################################################################
  # BEGIN RUBINIUS ( with Gemstone modifications)
  ######################################################################

  include Comparable

  ZoneOffset = {
      'UTC' => 0,
      # ISO 8601
      'Z' => 0,
      # RFC 822
      'UT' => 0, 'GMT' => 0,
      'EST' => -5, 'EDT' => -4,
      'CST' => -6, 'CDT' => -5,
      'MST' => -7, 'MDT' => -6,
      'PST' => -8, 'PDT' => -7,
      # Following definition of military zones is original one.
      # See RFC 1123 and RFC 2822 for the error in RFC 822.
      'A' => +1, 'B' => +2, 'C' => +3, 'D' => +4,  'E' => +5,  'F' => +6,
      'G' => +7, 'H' => +8, 'I' => +9, 'K' => +10, 'L' => +11, 'M' => +12,
      'N' => -1, 'O' => -2, 'P' => -3, 'Q' => -4,  'R' => -5,  'S' => -6,
      'T' => -7, 'U' => -8, 'V' => -9, 'W' => -10, 'X' => -11, 'Y' => -12,
  }

  MonthValue = {
    'JAN' => 1, 'FEB' => 2, 'MAR' => 3, 'APR' => 4, 'MAY' => 5, 'JUN' => 6,
    'JUL' => 7, 'AUG' => 8, 'SEP' => 9, 'OCT' =>10, 'NOV' =>11, 'DEC' =>12
  }

  LeapYearMonthDays = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  CommonYearMonthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

  FULL_DAY_NAME = ['Sunday, Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']

  FULL_MONTH_NAME = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']

  RFC2822_DAY_NAME = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']

  RFC2822_MONTH_NAME = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

  PRE_EPOCH_DAYS = 719468

  TM_FIELDS = {
    :sec => 0,
    :min => 1,
    :hour => 2,
    :mday => 3,
    :mon => 4,
    :year => 5,
    :wday => 6,
    :yday => 7,
    :isdst => 8,
  }

  # TIMEVAL_FIELDS = {   # not used in Gemstone
  #  :sec => 0,
  #  :usec => 1,
  #}

  #--
  # TODO: doesn't load nsec or ivars
  #++

  def self._load(data)  # used by Marshal
    raise TypeError, 'marshaled time format differ' unless data.length == 8

    major, minor = data.unpack 'VV'

    if (major & (1 << 31)) == 0 then
      at major, minor
    else
      major &= ~(1 << 31)

      is_gmt =  (major >> 30) & 0x1
      year   = ((major >> 14) & 0xffff) + 1900
      mon    = ((major >> 10) & 0xf) + 1
      mday   =  (major >>  5) & 0x1f
      hour   =  major         & 0x1f

      min   =  (minor >> 26) & 0x3f
      sec   =  (minor >> 20) & 0x3f
      isdst = false

      usec = minor & 0xfffff

      time = self.gm(year, mon, mday, hour, min, sec, usec)
      time.localtime # unless is_gmt.zero? # HACK MRI ignores the is_gmt flag
      time
    end
  end

  #--
  # TODO: doesn't dump nsec or ivars
  #++

  def _dump(limit = nil)  # used by marshal
    is_gmt = @_st_is_gmt
    unless is_gmt
      # Maglev deviation, always dumping in GMT format
      t = self.dup.gmtime
      return t._dump
    end
    tm = @_st_tm
    year = tm[TM_FIELDS[:year]]
    if (year & 0xffff) != year then
      raise ArgumentError, "year too big to marshal: #{year}"
    end

    major = 1                     << 31 | # 1 bit
      (is_gmt ? 1 : 0)     << 30 | # 1 bit
      tm[TM_FIELDS[:year]] << 14 | # 16 bits
      tm[TM_FIELDS[:mon]]  << 10 | # 4 bits
      tm[TM_FIELDS[:mday]] <<  5 | # 5 bits
      tm[TM_FIELDS[:hour]]         # 5 bits

    minor = tm[TM_FIELDS[:min]]  << 26 | # 6 bits
      tm[TM_FIELDS[:sec]] << 20 | # 6 bits
      self.usec  # 20 bits  # GEMSTONE changes

    [major, minor].pack 'VV'
  end

  def dup
    t = self.class.allocate   # Gemstone changes
    t.__init(@_st_microseconds, @_st_is_gmt)
  end

  def self.local(first, *args)
    if args.size == 9
      second = first
      minute = args[0]
      hour = args[1]
      day = args[2]
      month = args[3]
      year = args[4]
      #  args[5] is wday , not used	# gemstone comments
      #  args[6] is yday , not used
      isdst = args[7] ? 1 : 0
      #  args[8] is tz , not used yet
      usec = 0
    else
      year = first
      month = self.__monthname_to_num(args[0]) 
      day = args[1] || 1
      hour = args[2] || 0
      minute = args[3] || 0
      second = args[4] || 0
      usec = args[5] || 0
      isdst = -1  # ask C mktime() to determine dst
    end
    self.__mktime(second, minute, hour, day, month, year, usec, isdst, false)
  end

  def self.mktime(first, *args)
    self.local(first, *args)
  end

  def self.__monthname_to_num(ma)
      # resolve month names to numbers
      if ma._isInteger
        month = ma
      else
        ma = Maglev::Type.coerce_to_or_nil(ma, String, :to_str)
	if ma._isString
	  mint = ma.to_i
	  if mint._equal?(0)
	    month = MonthValue[ ma.upcase] || raise(ArgumentError.new('argument out of range'))
	  else
	    month = mint
	  end
	else
	  month = ma || 1
	end
      end
      month
  end

  def self.gm(first, *args)
    if args.size == 9
      second = first
      minute = args[0]
      hour = args[1]
      day = args[2]
      month = args[3]
      year = args[4]
      usec = 0
    else
      # set argument defaults
      year = first
      month = self.__monthname_to_num(args[0]) 
      day = args[1] || 1
      hour = args[2] || 0
      minute = args[3] || 0
      second = args[4] || 0
      usec = args[5] || 0
    end

    self.__mktime(second, minute, hour, day, month, year, usec, 0, true)
  end

  # Gemstone , new implementations at end of file
  #   def self.at ; end

  def inspect
    if @_st_is_gmt
      strftime("%a %b %d %H:%M:%S UTC %Y")
    else
      strftime("%a %b %d %H:%M:%S %z %Y")
    end
  end

  def seconds
    @_st_microseconds.__divide(1_000_000)   # Gemstone
  end

  def +(other)
    microsecs = @_st_microseconds
    if other._isInteger
      microsecs +=  other * 1_000_000
    elsif other._kind_of?(Time)
      raise TypeError , 'Time#+  , arg may not be a Time'
    else
      # addition with rounding to nearest microsecond
      other = Maglev::Type.coerce_to(other, Float, :to_f)
      if other > 0.0
        microsecs += ((other + 0.0000005) * 1_000_000.0).to_i
      elsif other <= 0.0
        unless other == 0.0
          microsecs += ((other - 0.0000005) * 1_000_000.0).to_i
        end
      end
   
    end
    t = self.class.allocate
    t.__init(microsecs, @_st_is_gmt)
  end

  def -(other)
    microsecs = @_st_microseconds
    if other._isInteger
      microsecs -=  other * 1_000_000
    elsif other._kind_of?(Time)
      delta = microsecs - other.__microsecs
      return (delta.to_f).__divide(1_000_000.0)
    else
      other = Maglev::Type.coerce_to(other, Float, :to_f)
      microsecs -= (other * 1_000_000.0).to_i
    end
    t = self.class.allocate
    t.__init(microsecs, @_st_is_gmt)
  end

  def succ
    self + 1
  end

  def <=>(other)
    if other._kind_of?(Time)
      @_st_microseconds <=> other.__microsecs 
    else
      nil
    end
  end

  # It seems that MRI return nil if other isn't a Time object
  def ==(other)
    result = self <=> other
    result._equal?(nil) ? result : result._equal?(0)
  end

  def hash
    micro_sec = @_st_microseconds
    (micro_sec.__divide(1_000_000)) ^ (micro_sec % 1_000_000) 
  end

  def eql?(other)
    (self <=> other)._equal?(0)
  end

  def asctime
    strftime("%a %b %e %H:%M:%S %Y")
  end

  def hour
    @_st_tm[2]
  end

  def min
    @_st_tm[1]
  end

  def sec
    @_st_tm[0]
  end

  def day
    @_st_tm[3]
  end

  def year
    @_st_tm[5] + 1900
  end

  def yday
    @_st_tm[7] + 1
  end

  def wday
    @_st_tm[6]
  end

  def zone
    strftime("%Z")
  end

  def mon
    @_st_tm[4] + 1
  end

  def gmt?
    @_st_is_gmt
  end

  def usec
    @_st_microseconds % 1_000_000 
  end

  def to_i
    @_st_microseconds.__divide(1_000_000)
  end

  def to_f
    @_st_microseconds.__divide(1_000_000.0)
  end

  ##
  # Returns:
  #   [ sec, min, hour, day, month, year, wday, yday, isdst, zone ]
  def to_a
    [sec, min, hour, day, month, year, wday, yday, isdst, zone]
  end

  def gmt_offset
    return 0 if @_st_is_gmt

    other = dup.gmtime

    if year != other.year
      offset = year < other.year ? -1 : 1
    elsif month != other.month
      offset = month < other.month ? -1 : 1
    elsif mday != other.mday
      offset = mday < other.mday ? -1 : 1
    else
      offset = 0
    end

    offset *= 24
    offset += hour - other.hour

    offset *= 60
    offset += min - other.min

    offset *= 60
    offset += sec - other.sec
  end

  def localtime
    if @_st_is_gmt
      self.__set_tmarray(false)
    end
    self
  end

  def gmtime
    unless @_st_is_gmt
      self.__set_tmarray(true)
    end 
    self
  end

  def dst?
    @_st_tm[8]._not_equal?(0)
  end

  def getlocal
    dup.localtime
  end

  def getgm
    dup.gmtime
  end

  def self.__to_int(arg)
    if arg._isString
      arg.to_i
    else
      Maglev::Type.coerce_to(arg, Integer, :to_int )
    end
  end

  def self.__mktime(sec, min, hour, mday, mon, year, usec, isdst=-1, from_gmt=false) 
    sec  = __to_int(sec)
    min  = __to_int(min)
    hour = __to_int(hour)
    mday = __to_int(mday)
    mon  = __to_int(mon)
    # range checks on above args done in C in __c_mktime 
    year = __to_int(year)
    usec = __to_int(usec)
    sec += usec.__divide(1000000)

    # This logic is taken from MRI, on how to deal with 2 digit dates.
    if year < 200
      if 0 <= year and year < 39
        warn "2 digit year used: #{year}"
        year += 2000
      elsif 69 <= year and year < 139
        warn "2 or 3 digit year used: #{year}"
        year += 1900
      end
    end
    year = year - 1900

    # build args to C mktime , args are in local time
    args = [ sec, min, hour, mday, mon, year, 
             0, # wday
             0, # yday, 
             isdst # negative means ask mktime() to determine Daylight/Standard
           ]
    atime_t = __c_mktime(args, from_gmt)
    t = self.allocate   
    microsecs = (atime_t * 1000000) + (usec % 1000000)
    t.__init(microsecs, from_gmt)
  end

  ##
  # +sec+ and +usec+ are always given in gmt here.
  #
  # +want_gmt+ says whether the caller wants a gmtime or local time object.

  def at_gmt(sec, usec, want_gmt)  # GEMSTONE
    if sec._isInteger 
      usec = usec ? usec.to_i : 0
    elsif usec
      sec  = Maglev::Type.coerce_to(sec, Integer, :to_i)
      usec = usec.to_i 
    else
      sec  = Maglev::Type.coerce_to(sec, Float, :to_f)
      usec = ((sec % 1) * 1_000_000).to_i
      sec  = sec.to_i
    end

    usec = usec + (sec * 1000000)

    @_st_microseconds =  usec

    if want_gmt
      force_gmtime
    else
      force_localtime
    end
  end


  def self.utc(first, *args)    # GEMSTONE
    gm(first, *args)
  end

  alias_method :utc?,       :gmt?
  alias_method :month,      :mon
  alias_method :ctime,      :asctime
  alias_method :mday,       :day
  alias_method :to_i,       :seconds
  alias_method :to_s,       :inspect
  alias_method :tv_sec,     :seconds
  alias_method :tv_usec,    :usec
  alias_method :utc,        :gmtime
  alias_method :isdst,      :dst?
  alias_method :utc_offset, :gmt_offset
  alias_method :gmtoff,     :gmt_offset
  alias_method :getutc,     :getgm

  ######################################################################
  # END Modified RUBINIUS
  ######################################################################

  # begin Gemstone  specific code

  class_primitive 'new'
  class_primitive 'now'
  # class_primitive_nobridge 'allocate' , '_basicNew'
  class_primitive_nobridge '__c_mktime' , 'mktime:fromGmt:'

  # __strftime takes a format String as the arg
  primitive_nobridge '__strftime' , 'strftime:'

  # Smalltalk initialize takes care of all instvars,
  #   for use cases Time.new  Time.now
  primitive_nobridge 'initialize'

  # __set_tmarray fills in  @is_gmt , @tm based on @microseconds 
  primitive_nobridge '__set_tmarray', '_setTmArray:'

  def __microsecs
    @_st_microseconds
  end

  def __init(aMicrosecs, isGmt)
    @_st_microseconds = aMicrosecs
    __set_tmarray(isGmt)
    self
  end

  def self.at(a_time)
    res = self.allocate
    if (a_time._kind_of?(self))
      usecs = a_time.__microsecs
    elsif a_time._isInteger
      usecs = a_time * 1_000_000
    else
      a_time = Maglev::Type.coerce_to(a_time, Float, :to_f)
      usecs = (a_time * 1000000.0).to_int
    end
    res.__init( usecs , false)
  end

  def self.at(secs, microsecs)
    res = self.allocate
    usecs = (secs.to_i * 1_000_000) + microsecs.to_i
    res.__init( usecs , false)
  end

  def strftime(fmt='%a %b %d %H:%M:%S %Z %Y' )
    fmt = fmt.to_str unless  fmt._isString
    __strftime(fmt)
  end

  def self.times
    Process.times
  end

end
Time.__freeze_constants

