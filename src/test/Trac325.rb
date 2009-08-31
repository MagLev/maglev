module Mha
  @@mywarn = 90
  def self.warning
    # if Gemstone.session_temp( :TrapCV ) ; nil.pause ; end
    x = @@mywarn
    @@mywarn = x + 10
    x
  end
end

class Haml
  include Mha
  def self.version
    y = defined?(@@version)
    if y
      return @@version
    else
      @@version = 88
      55
    end
  end
  def self.warning
    # if Gemstone.session_temp( :TrapCV ) ; nil.pause ; end
    x = @@mywarn
    @@mywarn = x + 1
    x
  end
end

hh = Haml
mm = Mha

a = Haml.version
b = Haml.version
unless a == 55 ; raise 'error' ; end
unless b == 88 ; raise 'error' ; end

c = Mha.warning 
unless c == 90 ; raise 'error' ; end

# Gemstone.session_temp_put( :TrapCV, true )
d = Haml.warning
unless d == 100 ; raise 'error' ; end

e = Mha.warning
unless e == 101 ; raise 'error' ; end

true

