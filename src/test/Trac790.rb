# MagLev generates an error with `...` or %x{ ... } if $? is non-zero:
#
#  $ maglev-ruby TracXXX.rb
#  #<Errno::EPERM: Error, 1, /usr/bin/env false>
#  /Users/pmclain/GemStone/dev/pbm.rb:8:in `raise_errno'
#  /Users/pmclain/GemStone/dev/pbm.rb:8:in `__system'
#  /Users/pmclain/GemStone/dev/pbm.rb:8
#  ERROR 2023, Error, 1, /usr/bin/env false (Errno::EPERM)

x = `/usr/bin/env true`     # This works ok
raise 'fail with output' unless x == ''
raise "Wrong exitstatus #{$?.exitstatus}" unless $?.exitstatus == 0

x = `/usr/bin/env false` # This fails
raise 'fail with output' unless x == ''
raise "Wrong exitstatus #{$?.exitstatus}" unless $?.exitstatus == 1

x = %x{ /usr/bin/env true }     # This works ok
raise 'fail with output' unless x == ''
raise "Wrong exitstatus #{$?.exitstatus}" unless $?.exitstatus == 0

x = %x{ /usr/bin/env false } # This fails
raise 'fail with output' unless x == ''
raise "Wrong exitstatus #{$?.exitstatus}" unless $?.exitstatus == 1

