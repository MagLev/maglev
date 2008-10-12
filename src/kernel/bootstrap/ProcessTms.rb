class ProcessTms
  # substitute implementation for Struct::Tms
  def initialize( p1, p2, p3, p4 )
    @utime = p1
    @stime = p2
    @cutime = p3
    @cstime = p4
  end

  def initialize(*args)
    @utime = args[0]
    @stime = args[1]
    @cutime = args[2]
    @cstime = args[3]
  end

  attr_accessor :utime, :stime, :cutime, :cstime
end

