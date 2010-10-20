
x = system('/usr/bin/env true')
puts "for true: x: #{x.inspect}   $?:  #{$?}  #{$?.exitstatus}"

raise "Fail A" unless x
gx = $?
raise "Fail B" unless $? == 0
raise "Fail C" unless (gy = $?.exitstatus) == 0

x = system('/usr/bin/env false')
puts "for false: x: #{x.inspect}   $?:  #{$?}  #{$?.exitstatus}"

gx = $?
raise "Fail A" if x
if $?.exitstatus == 255 # x86 solaris
  raise "Fail B" unless $? == 0xFF00
else
  raise "Fail C" unless (gy = $?.exitstatus) == 1
  raise "Fail B" unless $? == 256
end
true

