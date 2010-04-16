# This script iterates over a collection stored in the repository, and then
# prints out the number of objects this VM read in from the SPC.  It should
# be over 100K, as there are 100K objects in the collection.

require 'vsd'

raise "Need to run rake commit" unless defined?(Person) && defined?(Person::RANDOM_PEOPLE)

start_stats = VSD::SessionStats.my_statistics

total_age = 0
Person::RANDOM_PEOPLE.each {|p| total_age += p.age }
puts "Total age: #{total_age}"

stop_stats = VSD::SessionStats.my_statistics

diff_stats = stop_stats - start_stats
interesting_stats = ['ProcessName'] + VSD::SessionStats::STAT_NAMES.grep(/obj.*read/i)
interesting_stats.each do |s|
  puts "#{s}   ==> #{diff_stats[s]}"
end

raise 'Read too few objects' unless diff_stats['ObjectsRead'] > 100_000

