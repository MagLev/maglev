# If the following lines are uncommented, the whole file runs w/o raising a
# failure.  If they are left commented, then $? is messed up for the "fail c"
# check

# result = %x{ echo "hello" }
# raise "fail a:  result: #{result}" unless result == "hello\n"
# raise "fail b" unless $?.exitstatus == 0


# Code that fails or not, depending on if above are commented or not
%x{ echo "hello" }
raise "fail c" unless $?.exitstatus == 0
true
