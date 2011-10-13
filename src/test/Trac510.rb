#
# The problem is that the error reporting isn't using the Ruby classes.
#
# The begin...rescue code below shows that Maglev raises the proper
# Errno::ENOENT exception, and that the message is the correct format.
#
# But, when the error is reported out from topaz, it is all messed up.
# Instead of:
#
#  "-e:1:in `size': No such file or directory - /testing (Errno::ENOENT)\n\tfrom -e:1\n"
#
# we get:
#
#   "\nERROR 2023, /testingError, 2\n"
#
begin
  puts "File size of non-existing file is: #{File.new('/testing')}"
rescue Errno::ENOENT => e1
  m = e1.message
  raise "Bad message" unless m == "No such file or directory - /testing"
rescue Exception => e
  puts "#{e.class}: #{e.inspect}"
  raise "Failed: expecting Errno::ENOENT but got #{e.class}"
end
puts "File size of non-existing file is: #{File.new('/testing')}"
#mri    = `ruby        -e 'p File.size(\"/testing\")' 2>&1`
#maglev = `maglev-ruby -e 'p File.size(\"/testing\")' 2>&1`
#raise "Ruby Maglev Mismatch" unless mri == maglev
#################### Trac Info
# ID:         510
# Summary:    Asking for the size of  non-existent file behaves differntly
# Changetime: 2009-11-03 21:28:53+00:00
###

#  Rails uses this to work with log files.  The file non_existent_file_test.rb has the following line:
#  
#  puts "File size of non-existing file is: #{File.new('/testing').size}"
#  
#  MBP:feature_tests lattam$ ruby non_existent_file_test.rb
#  non_existent_file_test.rb:1:in `initialize': No such file or directory - /testing (Errno::ENOENT)
#  	from non_existent_file_test.rb:1:in `new'
#  	from non_existent_file_test.rb:1
#  MBP:feature_tests lattam$ maglev-ruby non_existent_file_test.rb
#  ERROR 2023, Error, 'Error,  nil '
#  
#  