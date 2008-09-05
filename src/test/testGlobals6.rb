class CA
  RuntimeArg = []
  NoArg = [ 5 ]
  class CB
    class NoArg < self
    end
  end
  NoArg.each { | e | RuntimeArg << CB::NoArg }
end
r = CA::NoArg  
unless r == [ 5 ]
  raise 'ERROR'
end
r = CA::RuntimeArg
# puts r.length
# puts r[0].class
unless r == [ CA::CB::NoArg ]
  raise 'ERROR'
end
# puts r.length
# puts 'yyy'
r = CA::CB::NoArg 
unless r == CA::CB::NoArg
  # puts r.class
  raise 'ERROR'
end
true
