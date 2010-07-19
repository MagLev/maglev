# Tests for Process and Proces::*

# Process::Status#== should compare to a number
result = %x{ echo "hello" }
raise "fail a:  result: #{result}" unless result == "hello\n"
raise "fail b" unless $?.exitstatus == 0
raise "fail c" unless $? == 0

