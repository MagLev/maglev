# This file holds test cases that are currently broken.  When they get
# fixed, they should be moved into FixedRegressions.rb, which is included
# in vmunit.conf, so that we can ensure we don't regress on these ad-hoc
# cases.

begin
  ary = [1,2,3]
  ary["cat"]
rescue TypeError
  # Nothing
rescue Exception => e
  puts "non TypeError unacceptable...#{e}"
end

