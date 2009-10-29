require 'benchmark'

root = Maglev::PERSISTENT_ROOT[:Person]

num_people = root.size
Benchmark.bm do |x|
  puts "Query each of #{num_people} people for all friends"
  x.report do
    count = 0
    root.each do |p|
      count += p.friends.size
    end
    puts "-- total friends #{count}"
    puts "-- average friends / person #{count.to_f / num_people.to_f}"
  end
end
