#!/usr/bin/ruby
Dir.glob("*test_case.rb").reject {|potential_test_case| potential_test_case =~ /common/}.each do |test_case| 
  require test_case 
  puts "Pulled in #{test_case}"
end
