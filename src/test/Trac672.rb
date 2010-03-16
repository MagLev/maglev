# MagLev is finding constants when it shouldn't
# Found in rubygems 1.3.6

XYZ = 10

module Foo
  ax = defined? XYZ     # should be "constant"
  unless ax == 'constant'; raise 'error'; end
  bx = defined? ::XYZ   # should be "constant"
  unless bx == 'constant'; raise 'error'; end
  begin
    cx = Foo::XYZ
    raise 'error , Foo::XYZ should not be defined'
  rescue NameError
    # ok
  end
  dx = defined?(Foo::XYZ)
  unless dx.equal?(nil) ; raise 'error'; end
end
true
