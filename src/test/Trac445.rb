# Trac 445,  problems with class variables in Modules

module Mod445
  @@var = 'Mod445_clsvar'
  def cvar
    @@var
  end
  def self.cvar
    @@var
  end
end

class Test445
  @@var = 'Test445_clsvar'
  extend Mod445
  def self.cvar
    @@var
  end
end

class Test445_2
  extend Mod445
end

a = Test445.cvar
c = Mod445.cvar
bcl = Test445_2
b = Test445_2.cvar
unless [a,b,c] == [ "Test445_clsvar", "Mod445_clsvar", "Mod445_clsvar"]
  raise 'error'
end
puts 'ok'
true
