module Mha
  @@mywarn = 90
  def warning
    @@mywarn
  end
end

class Haml
  include Mha
  @@mywarn = 99
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
    @@mywarn
  end
end

a = Haml.version
b = Haml.version
unless a == 55 ; raise 'error' ; end
unless b == 88 ; raise 'error' ; end

c = Haml.warning
unless c == 99 ; raise 'error' ; end

d = Haml.new.warning
unless d == 90 ; raise 'error' ; end

true

