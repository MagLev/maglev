
module Mod
  autoload("Class", "Trac450_test1")
end

Mod::Class.superclass

o = Mod::Class.new
unless o.method_2 == 'end of 450' ; raise 'error'; end
puts "Done 450"
true
