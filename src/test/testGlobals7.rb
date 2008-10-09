NoArg = 4
class CB
  BCx = 95
  class NoArg < self
  end

  def getBcx
    return self.class::BCx
  end
end

r = NoArg  # expect 4
unless r == 4
  raise 'ERROR'
end
r = CB::NoArg  
rcls = r.class
n = rcls.name
unless n == 'Class'
  puts r
  raise 'ERROR'
end

c = CB.new
r = c.getBcx 
unless r == 95
  raise 'ERROR'
end

true
