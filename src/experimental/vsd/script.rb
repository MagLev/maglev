require 'vsd'

raise "Need to run rake commit" unless defined?(Person) && defined?(Person::RANDOM_PEOPLE)

start_stats = VSD::SessionStats.all_statistics

total_age = 0
Person::RANDOM_PEOPLE.each {|p| total_age += p.age }
puts "Total age: #{total_age}"

stop_stats = VSD::SessionStats.all_statistics

both_stats = stop_stats.zip start_stats
diff_stats = stop_stats - start_stats

interesting_stats = ['ProcessName'] + VSD::SessionStats::STAT_NAMES.grep(/obj.*read/i)
interesting_stats.each do |s|
  puts "#{s}   ==> #{diff_stats[s]}"
end

