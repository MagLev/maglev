# This file holds test cases that are currently broken.  When they get
# fixed, they should be moved into FixedRegressions.rb, which is included
# in vmunit.conf, so that we can ensure we don't regress on these ad-hoc
# cases.

# The Rubinius Struct.rb does this
class Foo
  class << self
    alias_method :my_new, :new
  end
end

