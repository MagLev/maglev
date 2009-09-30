# Incorrect: returns nil, not the singleton class
oc = class << Object; end
p oc
unless oc.equal?(nil) ; raise 'error'; end
unless oc.is_a?(Object) ; raise 'error'; end
if oc.is_a?(Class) ; raise 'error'; end
if oc.is_a?(Module) ; raise 'error'; end

puts "---"
# Correct: Does return the singleton class
oc2 = class << Object; self; end
p oc2
unless oc2.name == '' ; raise 'error'; end
unless oc2.inspect == '#<Class:Object>' ; raise 'error'; end
unless oc2.is_a?(Object) ; raise 'error'; end
unless oc2.is_a?(Class) ; raise 'error'; end
unless oc2.is_a?(Module) ; raise 'error'; end
puts "Done"
