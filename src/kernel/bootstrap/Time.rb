# Time in Ruby is identically Smalltalk RubyTime
#
# The Smalltalk RubyTime class holds a single value, @microseconds, which
# is the number of microseconds since the epoch.  The usec() method just
# gets the fractional part of @microseconds.

class Time

  # time_switch is a rubinius builtin that modifies self to be either gmt
  # or localtime depending on +to_gmt+ flag.  (Possibly) changes the
  # reciever.
  # TODO: This is a stub...
  def time_switch(to_gmt)
    _stub_warn("Time#time_switch: does not convert between localtime and gmt")
  end

  ######################################################################
  # BEGIN RUBINIUS
  ######################################################################

  #  include Comparable

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

      time = gm year, mon, mday, hour, min, sec, usec
      time.localtime # unless is_gmt.zero? # HACK MRI ignores the is_gmt flag
      time
    end
  end

  #--
  # TODO: doesn't dump nsec or ivars
  #++

  def _dump(limit = nil)
    tm = @tm
    is_gmt = @is_gmt

    time_switch true
    year = @tm[TM_FIELDS[:year]]

    if (year & 0xffff) != year then
      raise ArgumentError, "year too big to marshal: #{year}"
    end

    gmt = @is_gmt ? 1 : 0

    major = 1                     << 31 | # 1 bit
      (@is_gmt ? 1 : 0)     << 30 | # 1 bit
      @tm[TM_FIELDS[:year]] << 14 | # 16 bits
      @tm[TM_FIELDS[:mon]]  << 10 | # 4 bits
      @tm[TM_FIELDS[:mday]] <<  5 | # 5 bits
      @tm[TM_FIELDS[:hour]]         # 5 bits
    minor = @tm[TM_FIELDS[:min]]  << 26 | # 6 bits
      @tm[TM_FIELDS[:sec]] << 20 | # 6 bits
      self.usec  # 20 bits  # GEMSTONE changes

    [major, minor].pack 'VV'
  ensure
    @tm = tm
    @is_gmt = is_gmt
  end

  def dup
    # t = Time.allocate
    # t.instance_variable_set(:@timeval, @timeval)
    # t.instance_variable_set(:@tm, @tm)
    # t.instance_variable_set(:@is_gmt, @is_gmt)
    # GEMSTONE implementation:
    t = self.class.allocate
    t._init(@microseconds, @is_gmt);
    t
  end

  def self.local(first, *args)
    if args.size == 9
      second = first
      minute = args[0]
      hour = args[1]
      day = args[2]
      month = args[3]
      year = args[4]
      usec = 0
      isdst = args[7] ? 1 : 0
    else
      # resolve month names to numbers
      if args[0] && (args[0]._isString || args[0].respond_to?(:to_str))
        args[0] = args[0].to_str if args[0].respond_to?(:to_str)
        month = MonthValue[args[0].upcase] || args[0].to_i || raise(ArgumentError.new('argument out of range'))
      end

      year = first
      month ||= args[0] || 1
      day = args[1] || 1
      hour = args[2] || 0
      minute = args[3] || 0
      second = args[4] || 0
      usec = args[5] || 0
      isdst = -1
    end

    t = Time.allocate   # GEMSTONE changes
    aTime_t = mktime(second, minute, hour, day, month, year, usec, isdst, false)
    t._init(aTime_t, false)
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
      # resolve month names to numbers
      if args[0] && args[0].respond_to?(:to_str) && (args[0] = args[0].to_str).to_i.equal?(0)
        month = MonthValue[args[0].upcase] || raise(ArgumentError.new('argument out of range'))
      end

      # set argument defaults
      year = first
      month ||= args[0] || 1
      day = args[1] || 1
      hour = args[2] || 0
      minute = args[3] || 0
      second = args[4] || 0
      usec = args[5] || 0
    end

    t = Time.allocate   # GEMSTONE chagns
    aTime_t = mktime(second, minute, hour, day, month, year, usec, -1, true)
    t._init(aTime_t, true)
    t
  end

  # Gemstone , new implementations at end of file
  #   def self.at(secs_or_time, msecs = nil)
  #     if secs_or_time.kind_of? Time
  #       return secs_or_time.dup
  #     end
  #     Time.allocate.at_gmt(secs_or_time, msecs, false)
  #   end
  #   def strftime(format)
  #     __strftime__(@tm, format.to_str)
  #   end

  def inspect(touchedSet=nil)
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
    raise TypeError, 'time + time?' if other.kind_of?(Time)  # GEMSTONE ...
    if (other._isFloat)
      deltamicro = (other * 1_000_000.0).to_i
    else
      deltamicro = other * 1_000_000
    end
    t = self.class.allocate
    t._init(@microseconds + deltamicro, @is_gmt)
    t                                                   # ... GEMSTONE
  end

  def -(other)
    if other.kind_of? Time
      (@microseconds - other._microsecs) / 1_000_000.0
    else
      # other is number of seconds to subtract
      if other._isFloat
        deltamicro = (other * 1_000_000.0).to_i
      else
        deltamicro = other * 1_000_000
      end
      t = self.class.allocate
      t._init(@microseconds - deltamicro, @is_gmt)
    end
  end

  def succ
    self + 1
  end

  def <=>(other)
    if other.kind_of? Time
      @microseconds <=> other._microsecs  # GEMSTONE change
    else
      nil
    end
  end

  # It seems that MRI return nil if other isn't a Time object
  def ==(other)
    result = self <=> other
    result.nil? ? result : result.equal?(0)
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
    @microseconds % 1_000_000     # GEMSTONE
  end

  def to_i
    @microseconds / 1_000_000  # inline self.seconds      # GEMSTONE
  end

  def to_f
    @microseconds / 1_000_000.0   # GEMSTONE
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
    force_localtime if @is_gmt

    self
  end

  def gmtime
    force_gmtime unless @is_gmt

    self
  end

  def dst?
    !@tm[8].zero?
  end

  def getlocal
    dup.localtime
  end

  def getgm
    dup.gmtime
  end

  def hash
    seconds ^ usec
  end

  def force_localtime
    time_switch false
    @is_gmt = false

    self
  end

  def force_gmtime
    time_switch true
    @is_gmt = true

    self
  end

  def self.mktime(sec, min, hour, mday, mon, year, usec, isdst, from_gmt) # GEMSTONE
    # returns UTC microseconds since 1970       # GEMSTONE
    sec  = sec.to_i
    min  = min.to_i
    hour = hour.to_i
    mday = mday.to_i
    mon  = mon.to_i
    year = year.to_i
    usec = usec.to_i

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

    args = [ sec, min, hour, mday, mon, year, usec, isdst, from_gmt ] # GEMSTONE...
    aTime_t = _mktime(args)
    raise ArgumentError, "time out of range" if aTime_t < 0
    aTime_t                                             # ... GEMSTONE
  end

  ##
  # +sec+ and +usec+ are always given in gmt here.
  #
  # +want_gmt+ says whether the caller wants a gmtime or local time object.

  def at_gmt(sec, usec, want_gmt)  # GEMSTONE, not used
    if sec.kind_of?(Integer) || usec
      sec  = Type.coerce_to sec, Integer, :to_i
      usec = usec ? usec.to_i : 0
    else
      sec  = Type.coerce_to sec, Float, :to_f
      usec = ((sec % 1) * 1_000_000).to_i
      sec  = sec.to_i
    end

    sec  = sec + (usec / 1_000_000)
    usec = usec % 1000000

    @timeval = [sec, usec]

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
  # END RUBINIUS
  ######################################################################

  # begin Gemstone  specific code

  class_primitive 'new'
  class_primitive 'now'
  class_primitive_nobridge 'allocate' , '_basicNew'
  class_primitive_nobridge '_mktime' , 'mktime:'

  # _strftime takes a format String as the arg
  primitive_nobridge '_strftime' , 'strftime:'

  # Smalltalk initialize takes care of all instvars,
  #   for use cases Time.new  Time.now
  primitive_nobridge 'initialize'

  # _setTmArray fills in  @is_gmt , @tm
  primitive_nobridge '_setTmArray', '_setTmArray:'

  def self.name
    # override Smalltalk name
    :Time
  end

  def _microsecs
    @microseconds
  end

  def _init(aMicrosecs, isGmt)
    @microseconds = aMicrosecs
    _setTmArray(isGmt);
  end

  def self.at(aTime)
    res = self.allocate
    if (aTime.kind_of?(self))
      usecs = aTime._microsecs
    else
      usecs = (aTime.to_i) * 1_000_000
    end
    res._init( usecs , false)
    res
  end

  def self.at(secs, microsecs)
    res = self.allocate
    usecs = (secs.to_i * 1_000_000) + microsecs.to_i
    res._init( usecs , false)
    res
  end

  # TODO httpdate

  def strftime(fmt='%a %b %d %H:%M:%S %Z %Y' )
    fmt = fmt.to_str unless  fmt._isString
    _strftime(fmt)
  end

end
