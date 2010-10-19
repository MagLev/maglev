
x = system('/usr/bin/env true')
puts "for true: x: #{x.inspect}   $?:  #{$?}  #{$?.exitstatus}"

raise "Fail A" unless x
raise "Fail B" unless $? == 0
raise "Fail C" unless $?.exitstatus == 0

x = system('/usr/bin/env false')
puts "for false: x: #{x.inspect}   $?:  #{$?}  #{$?.exitstatus}"

raise "Fail A" if x
raise "Fail B" unless $? == 256
raise "Fail C" unless $?.exitstatus == 1
