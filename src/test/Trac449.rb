# Incorrect: returns nil, not the singleton class
oc = class << Object; end
p oc
p oc.is_a?(Object)
p oc.is_a?(Class)
p oc.is_a?(Module)

puts "---"
# Correct: Does return the singleton class
oc2 = class << Object; self; end
p oc2
p oc2.is_a?(Object)
p oc2.is_a?(Class)
p oc2.is_a?(Module)

