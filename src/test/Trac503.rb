#
# Original problem:
#
#   $ maglev-ruby src/test/Trac503.rb
#     ERROR 2013, aC cannot perform the selector #'method_missing_test'
#     with the arguments in anArray( ). Perform may have been attempted
#     with wrong number of args.aC cannot perform the selector
#     #'method_missing_test' with the arguments in anArray( ).
#     Perform may have been attempted with wrong number of args.
#

class C503
  def method_missing(symbol, *args)
    puts "Method missing for #{symbol.inspect} called: #{args.inspect}"
    $count += 1
  end
end

$count = 0
c = C503.new
c.send :method_missing_test
c.send(:method_missing_test, [0, 1])
c.send(:method_missing_test, 0) { true }

expected = 3
raise "Fail expected #{expected} was #{$count}" unless $count == expected
true
#################### Trac Info
# ID:         503
# Summary:    Method missing implementation problem
# Changetime: 2009-04-29 16:43:32+00:00
###

#  The current implementation of method_missing uses an exception, but this exception gets caught by Ruby level exception handling!!!!  This needs to be fixed ASAP.
#  
#  MBP:feature_tests lattam$ ruby send_test.rb
#  Method missing for :method_missing_test called: []
#  MBP:feature_tests lattam$ maglev-ruby send_test.rb
#  Exception raised: aC cannot perform the selector #'method_missing_test' with the arguments in anArray( ). Perform may have been attempted with wrong number of args.
#  
#  