# Commit the KD Tree Code, then create and commit a tree of random
# locations.
require 'benchmark'
require 'utils'

if defined? Maglev
  raise "No Committed Data; run: 'rake commit' first." if Maglev::PERSISTENT_ROOT[:RANDOM_KDTREE].nil?
end

def time_queries(a_tree)
  num_queries = 1_000
  k = 100
  count = 0

  Benchmark.bm(15) do |r|
    r.report("Iterate Nodes") do
      a_tree.each {|el| count += 1 }
    end
  end

  tms = Benchmark.measure do
    num_queries.times { a_tree.nearest_k(Utils.random_point, k) }
  end

  times = [tms.utime, tms.stime, tms.total, tms.real]
  per_query_times = times.map {|el| el / num_queries }
  puts
  puts "#{num_queries} queries for #{k} nearest nodes"
  puts "               #{Benchmark::Tms::CAPTION}"
  puts "Time:          %10.6f %10.6f %10.6f (%10.6f)\n" % times
  puts "Per Query:     %10.6f %10.6f %10.6f (%10.6f)\n" % per_query_times

  fmt = count.to_s.gsub(/(\d)(?=(\d\d\d)+(?!\d))/, "\\1,")
  puts "\nTree has #{fmt} nodes"
end

if __FILE__ == $0
  if defined? Maglev
    time_queries Maglev::PERSISTENT_ROOT[:RANDOM_KDTREE]
  end
end
