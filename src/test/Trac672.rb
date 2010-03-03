# MagLev is finding constants when it shouldn't
# Found in rubygems 1.3.6

XYZ = 10

module Foo
  p defined? XYZ     # should be "constant"
  p defined? ::XYZ   # should be "constant"
  p Foo::XYZ         # should be nil, but is 10
  raise "XYZ should not be defined" if defined? Foo::XYZ
end
