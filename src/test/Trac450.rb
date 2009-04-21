
module Mod
  autoload("Class", File.expand_path('Trac450_test1.rb', File.dirname(__FILE__)))
end

Mod::Class.superclass

o = Mod::Class.new
unless o.method_2 == 'end of 450' ; raise 'error'; end
puts "Done 450"
true
