$dir = File.dirname(__FILE__)
require File.expand_path('simple', $dir)

module A
  autoload :B, "#{$dir}/AutoloadHelper.rb"
end

test(defined? :A,   true,  ":A should be defined before")
test(defined? A::B, false, "A::B should not be defined before autoload")
test(defined? A::B, false, "calling defined? should NOT have defined A::B")
test($marker, nil, "$marker should be nil before the autoload")

# Now reference A::B to force the autoload
b = A::B.new

test(defined? :A, true,   ":A should be defined after")
test(defined? A::B, true, "A::B should be defined after autoload")
test($marker, true, "$marker should not be nil after the autoload")
