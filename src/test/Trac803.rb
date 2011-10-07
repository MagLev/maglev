
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

#################### Trac Info
# ID:         803
# Summary:    Examples are broken when running Rake in MagLev
# Changetime: 2010-10-22 17:55:30+00:00
###

#  rake -T works, rake test usually works, but many other rake tasks in the examples either hang or get #<SystemExit: SystemExit Error,  9>
#  when run with rake installed in MagLev.
#  
#  You can see this by running rake in examples/sinatra/simple_blog,
#  examples/rack, examples/persistence/kdtree and likely others.