# Time in Ruby is identically Smalltalk RubyTime

class Time
  self.class.primitive 'new'
  self.class.primitive 'now' 
  self.class.primitive_nobridge 'allocate' , '_basicNew'

  # Smalltalk initialize takes care of all instvars,
  #   for use cases Time.new  Time.now
  primitive_nobridge 'initialize'

  # _setTmArray fills in  @is_gmt , @tm 
  primitive_nobridge '_setTmArray', '_setTmArray:'

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
      usecs = (aTime.to_i) * 1000000
    end
    res._init( usecs , false)
    res
  end

  def self.at(secs, microsecs)
    res = self.allocate
    usecs = (secs.to_i * 1000000) + microsecs.to_i 
    res._init( usecs , false)
    res
  end
  

  def ==(aTime)
    @microseconds == aTime.usec
  end 

  def -(other)
    to_f - other.to_f
  end
  
  primitive 'hash'
  
  def to_f
    (@microseconds / 1000000.0).to_f
  end

  # TODO httpdate

  # TODO 'year
  # TODO 'month
  # TODO 'day
  # TODO 'hour
  # TODO 'minute
  # TODO 'second
  # TODO 'monthName
  
  def strftime(str)
    str.gsub(/%Y/, year.to_s).gsub(/%m/, month.to_s).gsub(/%d/, day.to_s).gsub(/%H/, hour.to_s).gsub(/%M/, minute.to_s).gsub(/%S/, second.to_s).gsub(/%b/, monthName).gsub(/%Z/, "")
  end
  
  def inspect
    # TODO change to call httpdate
    # httpdate

    # TODO delete following after httpdate working again
    @microseconds.to_s + ':' + (@is_gmt ? 'gmt' : 'pst' )
  end
  
end
