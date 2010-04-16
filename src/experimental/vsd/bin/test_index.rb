# This script does some index lookups and set intersections and tries to
# prove that not too many objects are read into the VM.
# There are 100K objects in the collection.

require 'vsd'

raise "Need to run rake commit" unless defined?(Data) && defined?(Data::ALL_DATA)

def show_diff(s1, s2)
  puts "\n==== stats for #{s1['ProcessName']}"
  diff_stats = s1 - s2
  interesting_stats = ['ProcessName'] + VSD::SessionStats::STAT_NAMES.grep(/obj.*read/i)
  interesting_stats.each do |s|
    puts "#{s}   ==> #{diff_stats[s]}"
  end
end

start_stats = VSD::SessionStats.my_statistics
start_spc_stats = VSD::SessionStats.spc_statistics

young = Data::ALL_DATA.search([:'@age'], :lt, 25)

stop_stats = VSD::SessionStats.my_statistics
stop_spc_stats = VSD::SessionStats.spc_statistics


show_diff(stop_spc_stats, start_spc_stats)
show_diff(stop_stats, start_stats)
# heavy = Data::ALL_DATA.search([:'@weight'], :gt, 75)

# young_heavy = young & heavy


