class Process
  def self.times
    now = Time.now
    ProcessTms.new(now, 0.0, 0.0, 0.0)
  end
    
end

class ProcessTms
  attr_accessor :utime, :stime, :cutime, :cstime
  def initialize( p1, p2, p3, p4 )
    @utime = p1
    @stime = p2
    @cutime = p3
    @cstime = p4
  end
end