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
    'UTC' => 0, 'Z' => 0,  'UT' => 0, 'GMT' => 0,
    'EST' => -5, 'EDT' => -4, 'CST' => -6, 'CDT' => -5,
    'CET' => 1, 'CEST' => 2,
    'MST' => -7, 'MDT' => -6, 'PST' => -8, 'PDT' => -7,
    'A' => +1, 'B' => +2, 'C' => +3, 'D' => +4, 'E' => +5,
    'F' => +6, 'G' => +7, 'H' => +8, 'I' => +9, 'K' => +10,
    'L' => +11, 'M' => +12, 'N' => -1, 'O' => -2, 'P' => -3,
    'Q' => -4, 'R' => -5, 'S' => -6, 'T' => -7, 'U' => -8,
    'V' => -9, 'W' => -10, 'X' => -11, 'Y' => -12, }

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

  def self._load(data)
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

  def _dump(limit = nil)
    is_gmt = @is_gmt
    unless is_gmt
      t = self.dup.gmtime
      return t._dump
    end
    tm = @tm
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
    t._init(@microseconds, @is_gmt);
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
      month = self._monthname_to_num(args[0]) 
      day = args[1] || 1
      hour = args[2] || 0
      minute = args[3] || 0
      second = args[4] || 0
      usec = args[5] || 0
      isdst = -1  # ask C mktime() to determine dst
    end
    self._mktime(second, minute, hour, day, month, year, usec, isdst, false)
  end

  def self.mktime(first, *args)
    self.local(first, *args)
  end

  def self._monthname_to_num(ma)
      # resolve month names to numbers
      if ma._isInteger
        month = ma
      else
        ma = Type.coerce_to_or_nil(ma, String, :to_str)
	if ma._isString
	  mint = ma.to_i
	  if mint.equal?(0)
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
      month = self._monthname_to_num(args[0]) 
      day = args[1] || 1
      hour = args[2] || 0
      minute = args[3] || 0
      second = args[4] || 0
      usec = args[5] || 0
    end

    self._mktime(second, minute, hour, day, month, year, usec, 0, true)
  end

  # Gemstone , new implementations at end of file
  #   def self.at ; end

  def inspect
    if @is_gmt
      strftime("%a %b %d %H:%M:%S UTC %Y")
    else
      strftime("%a %b %d %H:%M:%S %z %Y")
    end
  end

  def seconds
    @microseconds / 1_000_000   # Gemstone
  end

  def +(other)
    microsecs = @microseconds
    if other._isInteger
      microsecs +=  other * 1_000_000
    elsif other.kind_of?(Time)
      raise TypeError , 'Time#+  , arg may not be a Time'
    else
      other = Type.coerce_to(other, Float, :to_f)
      microsecs += (other * 1_000_000.0).to_i
    end
    t = self.class.allocate
    t._init(microsecs, @is_gmt)
  end

  def -(other)
    microsecs = @microseconds
    if other._isInteger
      microsecs -=  other * 1_000_000
    elsif other.kind_of?(Time)
      delta = microsecs - other._microsecs
      return (delta.to_f) / 1_000_000.0
    else
      other = Type.coerce_to(other, Float, :to_f)
      microsecs -= (other * 1_000_000.0).to_i
    end
    t = self.class.allocate
    t._init(microsecs, @is_gmt)
  end

  def succ
    self + 1
  end

  def <=>(other)
    if other.kind_of?(Time)
      @microseconds <=> other._microsecs 
    else
      nil
    end
  end

  # It seems that MRI return nil if other isn't a Time object
  def ==(other)
    result = self <=> other
    result.nil? ? result : result.equal?(0)
  end

  def hash
    micro_sec = @microseconds
    (micro_sec / 1_000_000) ^ (micro_sec % 1_000_000) 
  end

  def eql?(other)
    (self <=> other).equal?(0)
  end

  def asctime
    strftime("%a %b %e %H:%M:%S %Y")
  end

  def hour
    @tm[2]
  end

  def min
    @tm[1]
  end

  def sec
    @tm[0]
  end

  def day
    @tm[3]
  end

  def year
    @tm[5] + 1900
  end

  def yday
    @tm[7] + 1
  end

  def wday
    @tm[6]
  end

  def zone
    strftime("%Z")
  end

  def mon
    @tm[4] + 1
  end

  def gmt?
    @is_gmt
  end

  def usec
    @microseconds % 1_000_000 
  end

  def to_i
    @microseconds / 1_000_000 
  end

  def to_f
    @microseconds / 1_000_000.0 
  end

  ##
  # Returns:
  #   [ sec, min, hour, day, month, year, wday, yday, isdst, zone ]
  def to_a
    [sec, min, hour, day, month, year, wday, yday, isdst, zone]
  end

  def gmt_offset
    return 0 if @is_gmt

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
    if @is_gmt
      self._set_tmarray(false);
    end
    self
  end

  def gmtime
    unless @is_gmt
      self._set_tmarray(true);
    end 
    self
  end

  def dst?
    @tm[8]._not_equal?(0)
  end

  def getlocal
    dup.localtime
  end

  def getgm
    dup.gmtime
  end

  def self._mktime(sec, min, hour, mday, mon, year, usec, isdst=-1, from_gmt=false) 
    sec  = Integer(sec)
    min  = Integer(min)
    hour = Integer(hour)
    mday = Integer(mday)
    mon  = Integer(mon)  # conversion to zero based done in C in _c_mktime
    # range checks on above args done in C in _c_mktime 
    year = Integer(year)
    usec = Integer(usec)
    sec += usec / 1000000  

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
    atime_t = _c_mktime(args, from_gmt)
    t = self.allocate   
    microsecs = (atime_t * 1000000) + (usec % 1000000)
    t._init(microsecs, from_gmt)
  end

  ##
  # +sec+ and +usec+ are always given in gmt here.
  #
  # +want_gmt+ says whether the caller wants a gmtime or local time object.

  def at_gmt(sec, usec, want_gmt)  # GEMSTONE
    if sec.kind_of?(Integer) || usec
      sec  = Type.coerce_to(sec, Integer, :to_i)
      usec = usec ? usec.to_i : 0
    else
      sec  = Type.coerce_to(sec, Float, :to_f)
      usec = ((sec % 1) * 1_000_000).to_i
      sec  = sec.to_i
    end

    usec = usec + (sec * 1000000)

    @microseconds =  usec

    if want_gmt
      force_gmtime
    else
      force_localtime
    end
  end


  def self.month_days(y, m)
    if ((y % 4 == 0) && (y % 100 != 0)) || (y % 400 == 0)
      LeapYearMonthDays[m-1]
    else
      CommonYearMonthDays[m-1]
    end
  end

  def self.apply_offset(year, mon, day, hour, min, sec, off)
    if off < 0
      off = -off
      off, o = off.divmod(60)
      if o != 0 then sec += o; o, sec = sec.divmod(60); off += o end
      off, o = off.divmod(60)
      if o != 0 then min += o; o, min = min.divmod(60); off += o end
      off, o = off.divmod(24)
      if o != 0 then hour += o; o, hour = hour.divmod(24); off += o end
      if off != 0
        day += off
        if month_days(year, mon) < day
          mon += 1
          if 12 < mon
            mon = 1
            year += 1
          end
          day = 1
        end
      end
    elsif 0 < off
      off, o = off.divmod(60)
      if o != 0 then sec -= o; o, sec = sec.divmod(60); off -= o end
      off, o = off.divmod(60)
      if o != 0 then min -= o; o, min = min.divmod(60); off -= o end
      off, o = off.divmod(24)
      if o != 0 then hour -= o; o, hour = hour.divmod(24); off -= o end
      if off != 0 then
        day -= off
        if day < 1
          mon -= 1
          if mon < 1
            year -= 1
            mon = 12
          end
          day = month_days(year, mon)
        end
      end
    end
    return year, mon, day, hour, min, sec
  end

    def rfc2822(date)
      if /\A\s*
          (?:(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)\s*,\s*)?
          (\d{1,2})\s+
          (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s+
          (\d{2,})\s+
          (\d{2})\s*
          :\s*(\d{2})\s*
          (?::\s*(\d{2}))?\s+
          ([+-]\d{4}|
           UT|GMT|EST|EDT|CST|CDT|MST|MDT|PST|PDT|[A-IK-Z])/ix =~ date
        # Since RFC 2822 permit comments, the regexp has no right anchor.
        day = $1.to_i
        mon = MonthValue[$2.upcase]
        year = $3.to_i
        hour = $4.to_i
        min = $5.to_i
        sec = $6 ? $6.to_i : 0
        zone = $7

        # following year completion is compliant with RFC 2822.
        year = if year < 50
                 2000 + year
               elsif year < 1000
                 1900 + year
               else
                 year
               end

        year, mon, day, hour, min, sec =
          apply_offset(year, mon, day, hour, min, sec, zone_offset(zone))
        t = self.utc(year, mon, day, hour, min, sec)
        t.localtime if !zone_utc?(zone)
        t
      else
        raise ArgumentError.new("not RFC 2822 compliant date: #{date.inspect}")
      end
    end
    alias rfc822 rfc2822


  # Returns a string which represents the time as date-time defined by RFC 2822:
  #
  #   day-of-week, DD month-name CCYY hh:mm:ss zone
  #
  # where zone is [+-]hhmm.
  #
  # If +self+ is a UTC time, -0000 is used as zone.
  #
  def rfc2822
    sprintf('%s, %02d %s %d %02d:%02d:%02d ',
      RFC2822_DAY_NAME[wday],
      day, RFC2822_MONTH_NAME[mon-1], year,
      hour, min, sec) +
    if utc?
      '-0000'
    else
      off = utc_offset
      sign = off < 0 ? '-' : '+'
      sprintf('%s%02d%02d', sign, *(off.abs / 60).divmod(60))
    end
  end
  alias rfc822 rfc2822

  # Returns a string which represents the time as rfc1123-date of HTTP-date
  # defined by RFC 2616:
  #
  #   day-of-week, DD month-name CCYY hh:mm:ss GMT
  #
  # Note that the result is always UTC (GMT).
  #
  def httpdate
    t = dup.utc
    sprintf('%s, %02d %s %d %02d:%02d:%02d GMT',
      RFC2822_DAY_NAME[t.wday],
      t.day, RFC2822_MONTH_NAME[t.mon-1], t.year,
      t.hour, t.min, t.sec)
  end

    # Parses +date+ as HTTP-date defined by RFC 2616 and converts it to a Time
    # object.
    #
    # ArgumentError is raised if +date+ is not compliant with RFC 2616 or Time
    # class cannot represent specified date.
    #
    # See #httpdate for more information on this format.
    #
    def httpdate(date)
      if /\A\s*
          (?:Mon|Tue|Wed|Thu|Fri|Sat|Sun),\x20
          (\d{2})\x20
          (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\x20
          (\d{4})\x20
          (\d{2}):(\d{2}):(\d{2})\x20
          GMT
          \s*\z/ix =~ date
        self.rfc2822(date)
      elsif /\A\s*
             (?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday),\x20
             (\d\d)-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-(\d\d)\x20
             (\d\d):(\d\d):(\d\d)\x20
             GMT
             \s*\z/ix =~ date
        year = $3.to_i
        if year < 50
          year += 2000
        else
          year += 1900
        end
        self.utc(year, $2, $1.to_i, $4.to_i, $5.to_i, $6.to_i)
      elsif /\A\s*
             (?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)\x20
             (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\x20
             (\d\d|\x20\d)\x20
             (\d\d):(\d\d):(\d\d)\x20
             (\d{4})
             \s*\z/ix =~ date
        self.utc($6.to_i, MonthValue[$1.upcase], $2.to_i,
                 $3.to_i, $4.to_i, $5.to_i)
      else
        raise ArgumentError.new("not RFC 2616 compliant date: #{date.inspect}")
      end
    end


  # Returns a string which represents the time as dateTime defined by XML
  # Schema:
  #
  #   CCYY-MM-DDThh:mm:ssTZD
  #   CCYY-MM-DDThh:mm:ss.sssTZD
  #
  # where TZD is Z or [+-]hh:mm.
  #
  # If self is a UTC time, Z is used as TZD.  [+-]hh:mm is used otherwise.
  #
  # +fractional_seconds+ specifies a number of digits of fractional seconds.
  # Its default value is 0.
  #
  def xmlschema(fraction_digits=0)
    sprintf('%d-%02d-%02dT%02d:%02d:%02d',
      year, mon, day, hour, min, sec) +
    if fraction_digits == 0
      ''
    elsif fraction_digits <= 9
      '.' + sprintf('%09d', nsec)[0, fraction_digits]
    else
      '.' + sprintf('%09d', nsec) + '0' * (fraction_digits - 9)
    end +
    if utc?
      'Z'
    else
      off = utc_offset
      sign = off < 0 ? '-' : '+'
      sprintf('%s%02d:%02d', sign, *(off.abs / 60).divmod(60))
    end
  end
  alias iso8601 xmlschema



  # GEMSTONE, not used
  #  public
  #
  #   class << self
  #     alias_method :now,    :new
  #     alias_method :mktime, :local
  #     alias_method :utc,    :gm
  #   end

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
  class_primitive_nobridge 'allocate' , '_basicNew'
  class_primitive_nobridge '_c_mktime' , 'mktime:fromGmt:'

  # _strftime takes a format String as the arg
  primitive_nobridge '_strftime' , 'strftime:'

  # Smalltalk initialize takes care of all instvars,
  #   for use cases Time.new  Time.now
  primitive_nobridge 'initialize'

  # _set_tmarray fills in  @is_gmt , @tm based on @microseconds 
  primitive_nobridge '_set_tmarray', '_setTmArray:'

  def _microsecs
    @microseconds
  end

  def _init(aMicrosecs, isGmt)
    @microseconds = aMicrosecs
    _set_tmarray(isGmt);
    self
  end

  def self.at(a_time)
    res = self.allocate
    if (a_time.kind_of?(self))
      usecs = a_time._microsecs
    elsif a_time._isInteger
      usecs = a_time * 1_000_000
    else
      a_time = Type.coerce_to(a_time, Float, :to_f)
      usecs = (a_time * 1000000.0).to_int
    end
    res._init( usecs , false)
  end

  def self.at(secs, microsecs)
    res = self.allocate
    usecs = (secs.to_i * 1_000_000) + microsecs.to_i
    res._init( usecs , false)
  end

  def strftime(fmt='%a %b %d %H:%M:%S %Z %Y' )
    fmt = fmt.to_str unless  fmt._isString
    _strftime(fmt)
  end

  def self.times
    Process.times
  end

end
Time._freeze_constants

