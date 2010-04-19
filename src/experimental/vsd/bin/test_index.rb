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

stats = VSD::StatSeries.my_series
stats.sample

young = Data::ALL_DATA.search([:'@age'],    :lt, 25)
light = Data::ALL_DATA.search([:'@weight'], :lt, 33)
young_and_light = young & light

stats.sample

puts "Num young #{young.size}  Num light #{light.size} Num young_and_light #{young_and_light.size}"
puts stats.diff.report(%w( ProcessName ObjectsRead ))


