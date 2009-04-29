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
