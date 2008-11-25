# Tests for methods in Kernel.rb
require File.expand_path('simple', File.dirname(__FILE__))

module M
end

# At this point, M.constants is:
#   MRI:    [ ]
#   maglev: [ "A" ]
test(M.constants, [], 'M.constants for empty module')

module M
  A = "A"
end
test(M.constants, ["A"], 'M.constants for A')

report
Gemstone.abortTransaction if defined? RUBY_ENGINE
true
