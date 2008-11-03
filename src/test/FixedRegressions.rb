# This file holds test cases that used to be in BrokenRegressions.rb, but
# have subsequently been fixed.  This file is run by vmunit.conf, so that
# we can ensure we don't regress on these ad-hoc cases.

# The Rubinius Struct.rb does this
class Foo
  class << self
    alias_method :my_new, :new
  end
end

##################################################

begin
  ary = [1,2,3]
  ary["cat"]
rescue TypeError
  # Nothing
rescue Exception => e
  puts "non TypeError unacceptable...#{e}"
end

