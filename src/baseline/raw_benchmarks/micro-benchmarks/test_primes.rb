# This tests the Prime class' generation speed.
# 
# Ruby 1.8 MRI has an extremely poor/naive implementation of Prime
# therefore it is expected to timeout on most machines for all but
# n = 3_000. The higher parameters are still useful to test Ruby 1.9
# and other implementations.
#
# Ruby 1.9 generates a warning about Prime::new being obsolete.
# It is used here to maintain compatibility with Ruby 1.8.

require 'mathn'

[3_000, 30_000, 300_000, 3_000_000].map do |n|
  p = Prime.new
  n.times { p.succ }
end
