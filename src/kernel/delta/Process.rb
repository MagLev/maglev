module Process
  def self.times
    arr = Gemstone._host_times
    Struct::Tms.new(*arr)
  end
end
